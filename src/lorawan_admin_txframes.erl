%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_txframes).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

-export([get_txframes/2]).

-include("lorawan.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_txframes}
    ], Req, State}.

get_txframes(Req, State) ->
    lorawan_admin:paginate(Req, State,
        read_txframes(lorawan_admin:get_filters(Req))).

read_txframes(Filter) ->
    lists:map(
        fun(Rec)-> lorawan_admin:build_txframe(Rec) end,
        mnesia:dirty_select(txframes, [{?to_record(txframe, lorawan_admin:parse_admin(Filter), '_'), [], ['$_']}])).

% end of file
