%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_database).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

-export([handle_get/2, handle_write/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

-record(state, {table, record, fields, key}).

init(Req, [Table, Record, Fields, text]) ->
    Key = cowboy_req:binding(hd(Fields), Req),
    {cowboy_rest, Req, #state{table=Table, record=Record, fields=Fields, key=Key}};
init(Req, [Table, Record, Fields, binary]) ->
    Key = case cowboy_req:binding(hd(Fields), Req) of
        undefined -> undefined;
        K -> lorawan_mac:hex_to_binary(K)
    end,
    {cowboy_rest, Req, #state{table=Table, record=Record, fields=Fields, key=Key}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, #state{key=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{key=undefined}=State) ->
    lorawan_admin:paginate(Req, State,
        lorawan_admin:sort(Req,
            read_records(lorawan_admin:get_filters(Req), State)));
handle_get(Req, #state{table=Table, key=Key}=State) ->
    [Rec] = mnesia:dirty_read(Table, Key),
    {jsx:encode(build_record(Rec, State)), Req, State}.

read_records(Filter, #state{table=Table, record=Record, fields=Fields}=State) ->
    Match = list_to_tuple([Record|[proplists:get_value(X, lorawan_admin:parse_admin(Filter), '_') || X <- Fields]]),
    lists:map(
        fun(Rec)-> build_record(Rec, State) end,
        mnesia:dirty_select(Table, [{Match, [], ['$_']}])).

build_record(Rec, #state{fields=Fields}) ->
    lorawan_admin:build_admin(
        lists:filter(fun({_, undefined}) -> false;
                        (_) -> true
                     end,
            lists:zip(Fields, tl(tuple_to_list(Rec))))).

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_write}
    ], Req, State}.

handle_write(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case jsx:is_json(Data) of
        true ->
            import_records(jsx:decode(Data, [{labels, atom}]), State),
            {true, Req2, State};
        false ->
            {stop, cowboy_req:reply(400, Req2), State}
    end.

import_records([], _State) -> ok;
import_records([First|Rest], State) when is_list(First) ->
    write_record(First, State),
    import_records(Rest, State);
import_records([First|_Rest] = List, State) when is_tuple(First) ->
    write_record(List, State).

write_record(List, #state{table=Table, record=Record, fields=Fields}) ->
    Rec = list_to_tuple([Record|[proplists:get_value(X, lorawan_admin:parse_admin(List)) || X <- Fields]]),
    mnesia:transaction(fun() ->
        ok = mnesia:write(Table, Rec, write) end).

resource_exists(Req, #state{key=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{table=Table, key=Key}=State) ->
    case mnesia:dirty_read(Table, Key) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, #state{table=Table, key=Key}=State) ->
    ok = mnesia:dirty_delete(Table, Key),
    {true, Req, State}.

% end of file
