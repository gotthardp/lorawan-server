%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_feed).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).
-export([notify/1]).

-include("lorawan.hrl").
-record(state, {table, fields, module, match, auth_fields}).

init(Req, {Table, Fields, AuthFields}) ->
    init0(Req, Table, Fields, lorawan_admin, AuthFields);
init(Req, {Table, Fields, Module, AuthFields}) ->
    init0(Req, Table, Fields, Module, AuthFields).

init0(Req, Table, Fields, Module, AuthFields) ->
    Filter = apply(Module, parse, [get_filters(Req)]),
    Match = list_to_tuple([Table|[maps:get(X, Filter, '_') || X <- Fields]]),
    % convert to websocket
    {ok, Timeout} = application:get_env(lorawan_server, websocket_timeout),
    {cowboy_websocket, Req,
        #state{table=Table, fields=Fields, module=Module, match=Match, auth_fields=AuthFields},
        #{idle_timeout => Timeout}}.

get_filters(Req) ->
    case cowboy_req:match_qs([{'_filters', [], <<"{}">>}], Req) of
        #{'_filters' := Filter} ->
            jsx:decode(Filter, [return_maps, {labels, atom}])
    end.

websocket_init(#state{table=Table} = State) ->
    lager:debug("Feed ~p connected", [Table]),
    ok = pg2:join({feed, Table}, self()),
    {reply, {text, encoded_records(State)}, State}.

websocket_handle({ping, _}, State) ->
    % no action needed as server handles pings automatically
    {ok, State};
websocket_handle(Data, State) ->
    lager:warning("Unknown handle ~w", [Data]),
    {ok, State}.

websocket_info({update, Scope}, #state{match=Match}=State) ->
    case lists_match(tuple_to_list(Scope), tuple_to_list(Match)) of
        true ->
            {reply, {text, encoded_records(State)}, State};
        false ->
            {ok, State}
    end;
websocket_info(Info, State) ->
    lager:warning("Unknown info ~p", [Info]),
    {ok, State}.

terminate(Reason, _Req, #state{table=Table}) ->
    lager:debug("Feed ~p terminated: ~p", [Table, Reason]),
    ok.

lists_match([A | RestA], [B | RestB])
        when A == '_' orelse B == '_' orelse A == B ->
    lists_match(RestA, RestB);
lists_match([], []) ->
    true;
lists_match(_, _) ->
    false.

notify(Scope) ->
    Table = element(1, Scope),
    case pg2:get_members({feed, Table}) of
        {error, _Error} ->
            ok;
        List when is_list(List) ->
            lists:foreach(
                fun(Pid) -> Pid ! {update, Scope} end,
                List)
    end.

encoded_records(State) ->
    jsx:encode(matched_records(State)).

matched_records(#state{table=Table, match=Match}=State) ->
    lists:map(
        fun(Rec)-> build_record(Rec, State) end,
        mnesia:dirty_select(Table, [{Match, [], ['$_']}])).

build_record(Rec, #state{fields=Fields, module=Module, auth_fields=AuthFields}) ->
    apply(Module, build, [
        maps:from_list(
            lists:filter(
                fun
                    % the module may not removed the null entries
                    ({_, undefined}) -> false;
                    ({Name, _}) -> lorawan_admin:auth_field(Name, AuthFields)
                end,
                lists:zip(Fields, tl(tuple_to_list(Rec)))))
        ]).

% end of file
