%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_links).

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

-record(state, {devaddr}).

init(Req, _Opts) ->
    DevAddr = case cowboy_req:binding(devaddr, Req) of
        undefined -> undefined;
        A -> lorawan_mac:hex_to_binary(A)
    end,
    {cowboy_rest, Req, #state{devaddr=DevAddr}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, #state{devaddr=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{devaddr=undefined}=State) ->
    lorawan_admin:paginate(Req, State, read_links());
handle_get(Req, #state{devaddr=DevAddr}=State) ->
    [Rec] = mnesia:dirty_read(links, DevAddr),
    {jsx:encode(build_link(Rec)), Req, State}.

read_links() ->
    lists:foldl(
        fun(Key, Acc)->
            [Rec] = mnesia:dirty_read(links, Key),
            [build_link(Rec)|Acc]
        end,
        [],
        mnesia:dirty_all_keys(links)).

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_write}
    ], Req, State}.

handle_write(Req, State) ->
    {ok, Data, Req2} = cowboy_req:read_body(Req),
    case jsx:is_json(Data) of
        true ->
            import_links(jsx:decode(Data, [{labels, atom}])),
            {true, Req2, State};
        false ->
            {stop, cowboy_req:reply(400, Req2), State}
    end.

import_links([]) -> ok;
import_links([First|Rest]) when is_list(First) ->
    add_link(First),
    import_links(Rest);
import_links([First|_Rest] = Data) when is_tuple(First) ->
    add_link(Data).

add_link(Data) ->
    Rec = parse_link(Data),
    mnesia:transaction(fun() ->
        ok = mnesia:write(links, Rec, write) end).

resource_exists(Req, #state{devaddr=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{devaddr=DevAddr}=State) ->
    case mnesia:dirty_read(links, DevAddr) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

delete_resource(Req, #state{devaddr=DevAddr}=State) ->
    ok = mnesia:dirty_delete(links, DevAddr),
    {true, Req, State}.

parse_link(List) ->
    ?to_record(link, lorawan_admin:parse_admin(List)).
build_link(Rec) ->
    lorawan_admin:build_admin(?to_proplist(link, Rec)).

% end of file
