%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_gateways).

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

-record(state, {mac}).

init(Req, _Opts) ->
    MAC = case cowboy_req:binding(mac, Req) of
        undefined -> undefined;
        M -> lorawan_mac:hex_to_binary(M)
    end,
    {cowboy_rest, Req, #state{mac=MAC}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, #state{mac=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{mac=undefined}=State) ->
    lorawan_admin:paginate(Req, State, read_gateways());
handle_get(Req, #state{mac=MAC}=State) ->
    [Rec] = mnesia:dirty_read(gateways, MAC),
    {jsx:encode(build_gateway(Rec)), Req, State}.

read_gateways() ->
    lists:foldl(
        fun(Key, Acc)->
            [Rec] = mnesia:dirty_read(gateways, Key),
            [build_gateway(Rec)|Acc]
        end,
        [],
        mnesia:dirty_all_keys(gateways)).

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_write}
    ], Req, State}.

handle_write(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case jsx:is_json(Data) of
        true ->
            import_gateways(jsx:decode(Data, [{labels, atom}])),
            {true, Req2, State};
        false ->
            {stop, cowboy_req:reply(400, Req2), State}
    end.

import_gateways([]) -> ok;
import_gateways([First|Rest]) when is_list(First) ->
    add_gateway(First),
    import_gateways(Rest);
import_gateways([First|_Rest] = Data) when is_tuple(First) ->
    add_gateway(Data).

add_gateway(Data) ->
    Rec = parse_gateway(Data),
    mnesia:transaction(fun() ->
        ok = mnesia:write(gateways, Rec, write) end).

resource_exists(Req, #state{mac=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{mac=MAC}=State) ->
    case mnesia:dirty_read(gateways, MAC) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, #state{mac=MAC}=State) ->
    ok = mnesia:dirty_delete(gateways, MAC),
    {true, Req, State}.

parse_gateway(List) ->
    ?to_record(gateway, lorawan_admin:parse_admin(List)).
build_gateway(Rec) ->
    lorawan_admin:build_admin(?to_proplist(gateway, Rec)).

% end of file
