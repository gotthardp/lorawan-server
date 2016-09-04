%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_gateway).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

-export([get_gateway/2]).
-export([write_gateway/2]).

-include("lorawan.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_gateway}
    ], Req, State}.

get_gateway(Req, User) ->
    [Rec] = mnesia:dirty_read(gateways, lorawan_mac:hex_to_binary(cowboy_req:binding(mac, Req))),
    {jsx:encode(lorawan_admin:build_gateway(Rec)), Req, User}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, write_gateway}
    ], Req, State}.

write_gateway(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case jsx:is_json(Data) of
        true ->
            Rec = lorawan_admin:parse_gateway(jsx:decode(Data, [{labels, atom}])),
            mnesia:transaction(fun() ->
                ok = mnesia:write(gateways, Rec, write) end),
            {true, Req2, State};
        false ->
            {stop, cowboy_req:reply(400, Req2), State}
    end.

resource_exists(Req, State) ->
    case mnesia:dirty_read(gateways, lorawan_mac:hex_to_binary(cowboy_req:binding(mac, Req))) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, State) ->
    ok = mnesia:dirty_delete(gateways, lorawan_mac:hex_to_binary(cowboy_req:binding(mac, Req))),
    {true, Req, State}.

% end of file
