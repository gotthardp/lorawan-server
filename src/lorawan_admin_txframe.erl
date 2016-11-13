%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_txframe).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

-export([get_txframe/2]).

-include("lorawan.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_txframe}
    ], Req, State}.

get_txframe(Req, User) ->
    [Rec] = mnesia:dirty_read(txframes, lorawan_mac:hex_to_binary(cowboy_req:binding(frid, Req))),
    {jsx:encode(lorawan_admin:build_txframe(Rec)), Req, User}.

resource_exists(Req, State) ->
    case mnesia:dirty_read(txframes, lorawan_mac:hex_to_binary(cowboy_req:binding(frid, Req))) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, State) ->
    ok = mnesia:dirty_delete(txframes, lorawan_mac:hex_to_binary(cowboy_req:binding(frid, Req))),
    {true, Req, State}.

% end of file
