%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_users).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-export([get_users/2]).
-export([create_user/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_users}
    ], Req, State}.

get_users(Req, State) ->
    lorawan_admin:paginate(Req, State, read_users()).

read_users() ->
    lists:foldl(
        fun(Key, Acc)->
            [Rec] = mnesia:dirty_read(users, Key),
            [lorawan_admin:build_user(Rec)|Acc]
        end,
        [],
        mnesia:dirty_all_keys(users)).

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, create_user}
    ], Req, State}.

create_user(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case jsx:is_json(Data) of
        true ->
            import_users(jsx:decode(Data, [{labels, atom}])),
            {true, Req2, State};
        false ->
            {stop, cowboy_req:reply(400, Req2), State}
    end.

import_users([]) -> ok;
import_users([First|Rest]) when is_list(First) ->
    add_user(First),
    import_users(Rest);
import_users([First|_Rest] = Data) when is_tuple(First) ->
    add_user(Data).

add_user(Data) ->
    Rec = lorawan_admin:parse_user(Data),
    mnesia:transaction(fun() ->
        ok = mnesia:write(users, Rec, write) end).

% end of file
