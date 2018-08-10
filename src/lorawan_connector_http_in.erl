%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_http_in).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

-export([handle_downlink/2]).

-include("lorawan_db.hrl").
-record(state, {connector, bindings}).

init(Req, [Connector]) ->
    Bindings = lorawan_admin:parse(cowboy_req:bindings(Req)),
    {cowboy_rest, Req, #state{connector=Connector, bindings=Bindings}}.

is_authorized(Req, State) ->
    case lorawan_admin:handle_authorization(Req, {[], [{<<"device:send">>, '*'}]}) of
        {true, _AuthFields} ->
            {true, Req, State};
        Else ->
            {Else, Req, State}
    end.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"PUT">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"octet-stream">>, '*'}, handle_downlink},
        {{<<"application">>, <<"json">>, '*'}, handle_downlink},
        {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_downlink}
    ], Req, State}.

handle_downlink(Req, #state{connector=Connector, bindings=Bindings}=State) ->
    {ok, Msg, Req2} = cowboy_req:read_body(Req),
    case lorawan_connector:decode_and_downlink(Connector, Msg, Bindings) of
        ok ->
            {true, Req2, State};
        {error, {Object, Error}} ->
            lorawan_utils:throw_error(Object, Error),
            {stop, cowboy_req:reply(400, Req2), State};
        {error, Error} ->
            lorawan_utils:throw_error({connector, Connector#connector.connid}, Error),
            {stop, cowboy_req:reply(400, Req2), State}
    end.

resource_exists(Req, State) ->
    {true, Req, State}.

% end of file
