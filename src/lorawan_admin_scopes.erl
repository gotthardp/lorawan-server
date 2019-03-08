%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_scopes).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).

-export([handle_get/2]).

-include("lorawan_db.hrl").
-record(state, {name, scopes, auth_fields}).

init(Req, Scopes) ->
    Name = cowboy_req:binding(name, Req),
    {cowboy_rest, Req, #state{name=Name, scopes=Scopes}}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

is_authorized(Req, #state{scopes=Scopes}=State) ->
    case lorawan_admin:handle_authorization(Req, Scopes) of
        {true, AuthFields} ->
            {true, Req, State#state{auth_fields=AuthFields}};
        Else ->
            {Else, Req, State}
    end.

forbidden(Req, #state{auth_fields=AuthFields}=State) ->
    {lorawan_admin:fields_empty(AuthFields), Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{name=undefined}=State) ->
    Scopes = lorawan_http_registry:get(scopes),
    {jsx:encode([[{name, S}] || S <- Scopes]), Req, State};
handle_get(Req, #state{name=Name}=State) ->
    {jsx:encode([{name, Name}]), Req, State}.

% end of file
