%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_txframes).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

-export([handle_get/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

-record(state, {frid}).

init(Req, _Opts) ->
    FrID = case cowboy_req:binding(frid, Req) of
        undefined -> undefined;
        F -> lorawan_mac:hex_to_binary(F)
    end,
    {cowboy_rest, Req, #state{frid=FrID}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, #state{frid=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{frid=undefined}=State) ->
    lorawan_admin:paginate(Req, State,
        read_txframes(lorawan_admin:get_filters(Req)));
handle_get(Req, #state{frid=FrID}=State) ->
    [Rec] = mnesia:dirty_read(txframes, FrID),
    {jsx:encode(build_txframe(Rec)), Req, State}.

read_txframes(Filter) ->
    lists:map(
        fun(Rec)-> build_txframe(Rec) end,
        mnesia:dirty_select(txframes, [{?to_record(txframe, lorawan_admin:parse_admin(Filter), '_'), [], ['$_']}])).

resource_exists(Req, #state{frid=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{frid=FrID}=State) ->
    case mnesia:dirty_read(txframes, FrID) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, #state{frid=FrID}=State) ->
    ok = mnesia:dirty_delete(txframes, FrID),
    {true, Req, State}.

build_txframe(Rec) ->
    lorawan_admin:build_admin(?to_proplist(txframe, Rec)).

% end of file
