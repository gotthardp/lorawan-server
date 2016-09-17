%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_rxframes).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

-export([get_rxframes/2]).

-include("lorawan.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_rxframes}
    ], Req, State}.

get_rxframes(Req, User) ->
    lorawan_admin:paginate(Req, User, read_rxframes()).

read_rxframes() ->
    lists:foldl(
        fun(Key, Acc)->
            [Rec] = mnesia:dirty_read(rxframes, Key),
            [lorawan_admin:build_rxframe(Rec)|Acc]
        end,
        [],
        mnesia:dirty_all_keys(rxframes)).

% end of file
