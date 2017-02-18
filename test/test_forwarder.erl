%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(test_forwarder).

-export([push_and_pull/2, rxpk/1]).

push_and_pull(MAC, Data) ->
    case pull_data(MAC) of
        {ok, Socket} ->
            case push_data(MAC, Data) of
                ok ->
                    R = await_pull(Socket),
                    gen_udp:close(Socket),
                    R;
                Error ->
                    gen_udp:close(Socket),
                    Error
            end;
        Error ->
            Error
    end.

rxpk(Base64Data) ->
    Pk = [{modu, <<"LORA">>}, {tmst, 0}, {freq, 868.10}, {datr, <<"SF12BW125">>}, {data, Base64Data}],
    [{rxpk, [Pk]}].


push_data(MAC, Data) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    Token = crypto:rand_bytes(2),
    % PUSH_DATA
    ok = gen_udp:send(Socket, "localhost", 1680, <<1, Token:2/binary, 0, MAC/binary, (jsx:encode(Data))/binary>>),
    R = receive
            % PUSH_ACK
            {udp, Socket, _, _, <<1, Token:2/binary, 1>>} ->
                ok
            after 100 ->
                {error, timeout}
        end,
    gen_udp:close(Socket),
    R.

pull_data(MAC) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    Token = crypto:rand_bytes(2),
    % PULL_DATA
    ok = gen_udp:send(Socket, "localhost", 1680, <<1, Token:2/binary, 2, MAC/binary>>),
    receive
        % PULL_ACK
        {udp, Socket, _, _, <<1, Token:2/binary, 4>>} ->
            {ok, Socket}
        after 100 ->
            gen_udp:close(Socket),
            {error, timeout}
    end.

await_pull(Socket) ->
    receive
        % PULL_RESP
        {udp, Socket, _, _, <<1, _:16, 3, Data/binary>>} ->
            Pk = jsx:decode(Data, [{labels, atom}]),
            TxPk = proplists:get_value(txpk, Pk),
            {ok, {proplists:get_value(data, TxPk)}}
        after 100 ->
            {error, timeout}
    end.

% end of file
