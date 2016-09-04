%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_user).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

-export([get_user/2]).
-export([write_user/2]).

-include("lorawan.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_user}
    ], Req, State}.

get_user(Req, User) ->
    [Rec] = mnesia:dirty_read(users, cowboy_req:binding(name, Req)),
    {jsx:encode(lorawan_admin:build_user(Rec)), Req, User}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, write_user}
    ], Req, State}.

write_user(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case jsx:is_json(Data) of
        true ->
            Rec = lorawan_admin:parse_user(jsx:decode(Data, [{labels, atom}])),
            mnesia:transaction(fun() ->
                ok = mnesia:write(users, Rec, write) end),
            {true, Req2, State};
        false ->
            {stop, cowboy_req:reply(400, Req2), State}
    end.

resource_exists(Req, State) ->
    case mnesia:dirty_read(users, cowboy_req:binding(name, Req)) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, State) ->
    ok = mnesia:dirty_delete(users, cowboy_req:binding(name, Req)),
    {true, Req, State}.

% end of file
