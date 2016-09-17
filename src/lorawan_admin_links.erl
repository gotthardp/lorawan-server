%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_links).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

-export([get_links/2]).
-export([create_link/2]).

-include("lorawan.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_links}
    ], Req, State}.

get_links(Req, User) ->
    lorawan_admin:paginate(Req, User, read_links()).

read_links() ->
    lists:foldl(
        fun(Key, Acc)->
            [Rec] = mnesia:dirty_read(links, Key),
            [lorawan_admin:build_link(Rec)|Acc]
        end,
        [],
        mnesia:dirty_all_keys(links)).

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, create_link}
    ], Req, State}.

create_link(Req, State) ->
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
    Rec = lorawan_admin:parse_link(Data),
    mnesia:transaction(fun() ->
        ok = mnesia:write(links, Rec, write) end).

% end of file
