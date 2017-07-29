%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(test_forwarder).
-behaviour(gen_server).

-export([start_link/2, stop/1, push_and_pull/2, rxpk/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {mac, server, socket, motes, rxpks, push_tokens, pull_tokens}).
-include_lib("eunit/include/eunit.hrl").

-define(PUSH_TIMEOUT_MS, 100).
-define(PULL_TIMEOUT_MS, 200).

start_link(MAC, Server) ->
    gen_server:start_link(?MODULE, [MAC, Server], []).

stop(Gateway) ->
    gen_server:stop(Gateway).

push_and_pull(Gateway, Data) ->
    Gateway ! {uplink, self(), Data},
    receive
        Response -> Response
        after 2000 -> {error, timeout}
    end.

init([MAC, Server]) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    self() ! pull_data,
    {ok, #state{mac=MAC, server=Server, socket=Socket, motes=[], rxpks=[],
        push_tokens=sets:new(), pull_tokens=sets:new()}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

% uplinks
handle_info({uplink, _Pid, Binary}, State) when is_binary(Binary) ->
    {ok, State2} = push_data(Binary, State),
    {noreply, State2};
handle_info({uplink, Pid, {rxpk, Frame}}, #state{rxpks=[]}=State) ->
    {ok, _} = timer:send_after(?PUSH_TIMEOUT_MS, push_data),
    {noreply, store_rxpk(Pid, Frame, State)};
handle_info({uplink, Pid, {rxpk, Frame}}, #state{rxpks=Pks}=State) when length(Pks) < 50 ->
    {noreply, store_rxpk(Pid, Frame, State)};
handle_info({uplink, Pid, {rxpk, Frame}}, State) ->
    #state{rxpks=Pks2} = State2 = store_rxpk(Pid, Frame, State),
    {ok, State3} = push_data(jsx:encode([{rxpk, Pks2}]), State2),
    {noreply, State3#state{rxpks=[]}};

handle_info(push_data, #state{rxpks=Pks}=State) ->
    {ok, State2} = push_data(jsx:encode([{rxpk, Pks}]), State),
    {noreply, State2#state{rxpks=[]}};
% PUSH_ACK
handle_info({udp, Socket, _, _, <<1, Token:2/binary, 1>>},
        #state{socket=Socket, push_tokens=Tokens}=State) ->
    {noreply, State#state{push_tokens=sets:del_element(Token, Tokens)}};
handle_info({push_expired, Token}, #state{push_tokens=Tokens}=State) ->
    {noreply, State#state{push_tokens=sets:del_element(Token, Tokens)}};
% PULL_DATA
handle_info(pull_data, #state{mac=MAC, pull_tokens=Tokens}=State) ->
    Token = crypto:strong_rand_bytes(2),
    ok = send(State, <<1, Token:2/binary, 2, MAC/binary>>),
    {ok, _} = timer:send_after(?PULL_TIMEOUT_MS, {pull_expired, Token}),
    {ok, _} = timer:send_after(1000, pull_data),
    {noreply, State#state{pull_tokens=sets:add_element(Token, Tokens)}};
% PULL_ACK
handle_info({udp, Socket, _, _, <<1, Token:2/binary, 4>>},
        #state{socket=Socket, pull_tokens=Tokens}=State) ->
    {noreply, State#state{pull_tokens=sets:del_element(Token, Tokens)}};
handle_info({pull_expired, Token}, #state{pull_tokens=Tokens}=State) ->
    {noreply, State#state{pull_tokens=sets:del_element(Token, Tokens)}};
% PULL_RESP
handle_info({udp, Socket, _, _, <<1, _:16, 3, Data/binary>>},
        #state{socket=Socket, motes=Motes}=State) ->
    Pk = jsx:decode(Data, [{labels, atom}]),
    TxPk = proplists:get_value(txpk, Pk),
    Frame = proplists:get_value(data, TxPk),
    % send to the device that opened the window
    get_mote(proplists:get_value(tmst, TxPk), Motes) ! {ok, Frame},
    {noreply, State}.

store_rxpk(Mote, Frame, #state{motes=Motes, rxpks=Pks}=State) ->
    {Idx, Motes2} = store_mote(Mote, Motes),
    % we assign each mote a sequence number and the use it as a timestamp
    Frame2 = [{tmst, Idx} | Frame],
    State#state{motes=Motes2, rxpks=[Frame2 | Pks]}.

% PUSH_DATA
push_data(Payload, #state{mac=MAC, push_tokens=Tokens}=State) ->
    Token = crypto:strong_rand_bytes(2),
    ok = send(State, <<1, Token:2/binary, 0, MAC/binary, Payload/binary>>),
    {ok, _} = timer:send_after(100, {push_expired, Token}),
    {ok, State#state{push_tokens=sets:add_element(Token, Tokens)}}.

terminate(_Reason, #state{socket=Socket}) ->
    gen_udp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


store_mote(Mote, Motes) ->
    case lists:keyfind(Mote, 2, Motes) of
        {Idx, Mote} ->
            {Idx, Motes};
        false ->
            % unique id, assuming we don't delete entries
            NewIdx = length(Motes),
            {NewIdx, [{NewIdx, Mote} | Motes]}
    end.

get_mote(Time, Motes) ->
    Idx = if
        Time >= 2000000 -> Time - 2000000;
        Time >= 1000000 -> Time - 1000000
    end,
    case lists:keyfind(Idx, 1, Motes) of
        {Idx, Mote} ->
            Mote;
        false ->
            undefined
    end.

rxpk(Base64Data) ->
    {rxpk, [{modu, <<"LORA">>}, {freq, 868.10}, {datr, <<"SF12BW125">>}, {codr, <<"4/5">>},
        {data, Base64Data}]}.

send(#state{socket=Socket, server={IP, Port}}, Payload) ->
    gen_udp:send(Socket, IP, Port, Payload).

% end of file
