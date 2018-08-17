%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_applications).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([handle_get/2]).
-record(state, {name, scopes, auth_fields}).

-include("lorawan_db.hrl").

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
    {ok, Modules} = application:get_env(lorawan_server, applications),
    A = lists:map(
            fun({Name, _Module}) -> [{name, Name}] end,
            Modules),
    B = lists:map(
            fun(Name) -> [{name, Name}] end,
            mnesia:dirty_all_keys(handler)),
    {jsx:encode(A++B), Req, State};
handle_get(Req, #state{name=Name}=State) ->
    {jsx:encode([{name, Name}]), Req, State}.

resource_exists(Req, #state{name=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{name=Name}=State) ->
    {ok, Modules} = application:get_env(lorawan_server, applications),
    case proplists:is_defined(Name, Modules) of
        true ->
            {true, Req, State};
        false ->
            % if it's not internal, then it must be external
            case mnesia:dirty_read(handler, Name) of
                [#handler{}] ->
                    {true, Req, State};
                [] ->
                    {false, Req, State}
            end
    end.

% end of file
