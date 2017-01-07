%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_users).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

-export([handle_get/2, handle_write/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

-record(state, {name}).

init(Req, _Opts) ->
    Name = cowboy_req:binding(name, Req),
    {cowboy_rest, Req, #state{name=Name}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, #state{name=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{name=undefined}=State) ->
    lorawan_admin:paginate(Req, State, read_users());
handle_get(Req, #state{name=Name}=State) ->
    [Rec] = mnesia:dirty_read(users, Name),
    {jsx:encode(build_user(Rec)), Req, State}.

read_users() ->
    lists:foldl(
        fun(Key, Acc)->
            [Rec] = mnesia:dirty_read(users, Key),
            [build_user(Rec)|Acc]
        end,
        [],
        mnesia:dirty_all_keys(users)).

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_write}
    ], Req, State}.

handle_write(Req, State) ->
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
    Rec = parse_user(Data),
    mnesia:transaction(fun() ->
        ok = mnesia:write(users, Rec, write) end).

resource_exists(Req, #state{name=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{name=Name}=State) ->
    case mnesia:dirty_read(users, Name) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, #state{name=Name}=State) ->
    ok = mnesia:dirty_delete(users, Name),
    {true, Req, State}.

parse_user(List) ->
    ?to_record(user, lorawan_admin:parse_admin(List)).
build_user(Rec) ->
    lorawan_admin:build_admin(?to_proplist(user, Rec)).

% end of file
