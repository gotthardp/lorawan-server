%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_gateways).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-export([get_gateways/2]).
-export([create_gateway/2]).

-include("lorawan.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_gateways}
    ], Req, State}.

get_gateways(Req, User) ->
    {jsx:encode(read_gateways()), Req, User}.

read_gateways() ->
    lists:foldl(
        fun(Key, Acc)->
            [Rec] = mnesia:dirty_read(gateways, Key),
            [lorawan_admin:build_gateway(Rec)|Acc]
        end,
        [],
        mnesia:dirty_all_keys(gateways)).

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, create_gateway}
    ], Req, State}.

create_gateway(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case jsx:is_json(Data) of
        true ->
            import_gateways(jsx:decode(Data, [{labels, atom}])),
            {true, Req2, State};
        false ->
            {stop, cowboy_req:reply(400, Req2), State}
    end.

import_gateways([]) -> ok;
import_gateways([First|Rest]) when is_list(First) ->
    add_gateway(First),
    import_gateways(Rest);
import_gateways([First|_Rest] = Data) when is_tuple(First) ->
    add_gateway(Data).

add_gateway(Data) ->
    Rec = lorawan_admin:parse_gateway(Data),
    mnesia:transaction(fun() ->
        ok = mnesia:write(gateways, Rec, write) end).

% end of file
