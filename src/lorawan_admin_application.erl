%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_application).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_application/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_application}
    ], Req, State}.

get_application(Req, User) ->
    {jsx:encode([{name, cowboy_req:binding(name, Req)}]), Req, User}.

resource_exists(Req, State) ->
    {ok, Modules} = application:get_env(lorawan_server, plugins),
    {proplists:is_defined(cowboy_req:binding(name, Req), Modules), Req, State}.

% end of file
