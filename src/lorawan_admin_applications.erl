%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_applications).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([handle_get/2]).
-record(state, {name}).

init(Req, _Opts) ->
    Name = cowboy_req:binding(name, Req),
    {cowboy_rest, Req, #state{name=Name}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{name=undefined}=State) ->
    {ok, Modules} = application:get_env(lorawan_server, plugins),
    A = lists:map(
        fun({Name, _Module}) -> [{name, Name}] end,
        Modules),
    {jsx:encode(A), Req, State};
handle_get(Req, #state{name=Name}=State) ->
    {jsx:encode([{name, Name}]), Req, State}.

resource_exists(Req, #state{name=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{name=Name}=State) ->
    {ok, Modules} = application:get_env(lorawan_server, plugins),
    {proplists:is_defined(Name, Modules), Req, State}.

% end of file
