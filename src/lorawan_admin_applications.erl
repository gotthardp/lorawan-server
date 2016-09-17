%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_applications).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

-export([get_applications/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_applications}
    ], Req, State}.

get_applications(Req, User) ->
    {ok, Modules} = application:get_env(lorawan_server, plugins),
    A = lists:map(
        fun({Name, _Module}) -> [{name, Name}] end,
        Modules),
    {jsx:encode(A), Req, User}.

% end of file
