%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_db_field).

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

-record(state, {table, key, field, fidx, module}).

init(Req, [Table, Fields]) ->
    init0(Req, Table, Fields, lorawan_admin);
init(Req, [Table, Fields, Module]) ->
    init0(Req, Table, Fields, Module).

init0(Req, Table, Fields, Module) ->
    Key = lorawan_admin:parse_field(hd(Fields), cowboy_req:binding(hd(Fields), Req)),
    Field = binary_to_existing_atom(cowboy_req:binding(field, Req), latin1),
    {cowboy_rest, Req, #state{table=Table, key=Key,
        field=Field, fidx=lorawan_utils:index_of(Field, Fields), module=Module}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{table=Table, key=Key, field=Field, fidx=Idx, module=Module}=State) ->
    [Rec] = mnesia:dirty_read(Table, Key),
    Value = apply(Module, build_field, [Field, element(Idx+1, Rec)]),
    {jsx:encode(#{Field => Value}), Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_write}
    ], Req, State}.

handle_write(Req, #state{field=Field, module=Module}=State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case catch jsx:decode(Data, [return_maps, {labels, atom}]) of
        Struct when is_map(Struct) ->
            Value = apply(Module, parse_field, [Field, maps:get(Field, Struct, undefined)]),
            {atomic, ok} = update_record(Value, State),
            {true, Req2, State};
        _Else ->
            lager:debug("Bad JSON in HTTP request"),
            {stop, cowboy_req:reply(400, Req2), State}
    end.

resource_exists(Req, #state{fidx=undefined}=State) ->
    {false, Req, State};
resource_exists(Req, #state{table=Table, key=Key}=State) ->
    case mnesia:dirty_read(Table, Key) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, State) ->
    {atomic, ok} = update_record(undefined, State),
    {true, Req, State}.

update_record(Value, #state{table=Table, key=Key, fidx=Idx}) ->
    mnesia:transaction(
        fun() ->
            [Rec] = mnesia:read(Table, Key, write),
            Rec2 = setelement(Idx+1, Rec, Value),
            mnesia:write(Table, Rec2, write)
        end).

% end of file
