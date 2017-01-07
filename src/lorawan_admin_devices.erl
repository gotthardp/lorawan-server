%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_devices).

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

-record(state, {deveui}).

init(Req, _Opts) ->
    DevEUI = case cowboy_req:binding(deveui, Req) of
        undefined -> undefined;
        S -> lorawan_mac:hex_to_binary(S)
    end,
    {cowboy_rest, Req, #state{deveui=DevEUI}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, #state{deveui=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{deveui=undefined}=State) ->
    lorawan_admin:paginate(Req, State, read_devices());
handle_get(Req, #state{deveui=DevEUI}=State) ->
    [Rec] = mnesia:dirty_read(devices, DevEUI),
    {jsx:encode(build_device(Rec)), Req, State}.

read_devices() ->
    lists:foldl(
        fun(Key, Acc)->
            [Rec] = mnesia:dirty_read(devices, Key),
            [build_device(Rec)|Acc]
        end,
        [],
        mnesia:dirty_all_keys(devices)).

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_write}
    ], Req, State}.

handle_write(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case jsx:is_json(Data) of
        true ->
            import_devices(jsx:decode(Data, [{labels, atom}])),
            {true, Req2, State};
        false ->
            {stop, cowboy_req:reply(400, Req2), State}
    end.

import_devices([]) -> ok;
import_devices([First|Rest]) when is_list(First) ->
    add_device(First),
    import_devices(Rest);
import_devices([First|_Rest] = Data) when is_tuple(First) ->
    add_device(Data).

add_device(Data) ->
    Rec = parse_device(Data),
    mnesia:transaction(fun() ->
        ok = mnesia:write(devices, Rec, write) end).

resource_exists(Req, #state{deveui=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{deveui=DevEUI}=State) ->
    case mnesia:dirty_read(devices, DevEUI) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, #state{deveui=DevEUI}=State) ->
    ok = mnesia:dirty_delete(devices, DevEUI),
    {true, Req, State}.

parse_device(List) ->
    ?to_record(device, lorawan_admin:parse_admin(List)).
build_device(Rec) ->
    lorawan_admin:build_admin(?to_proplist(device, Rec)).

% end of file
