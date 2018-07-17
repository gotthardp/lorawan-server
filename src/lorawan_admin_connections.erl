%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_connections).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

-export([handle_get/2, handle_action/2]).

-include("lorawan_db.hrl").
-record(state, {app, action}).

init(Req, _Opts) ->
    App = cowboy_req:binding(app, Req),
    Action = cowboy_req:binding(action, Req),
    {cowboy_rest, Req, #state{app=App, action=Action}}.

is_authorized(Req, State) ->
    {lorawan_admin:handle_authorization(Req), Req, State}.

allowed_methods(Req, #state{action=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{app=undefined}=State) ->
    {jsx:encode(get_connections(pg2:which_groups(), [])), Req, State};
handle_get(Req, #state{app=App}=State) ->
    {jsx:encode(get_connection(App)), Req, State}.

get_connections([{backend, App}|More], Acc) ->
    get_connections(More, [get_connection(App)|Acc]);
get_connections([_Else|More], Acc) ->
    get_connections(More, Acc);
get_connections([], Acc)->
    Acc.

get_connection(App) ->
    case pg2:get_members({backend, App}) of
        List when is_list(List) ->
            #{app => App, count => length(List)};
        {error, _} ->
            #{app => App, count => 0}
    end.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_action}
    ], Req, State}.

handle_action(Req, #state{app=App, action = <<"send">>}=State) ->
    case lorawan_backend_factory:nodes_with_backend(App) of
        [#node{devaddr=DevAddr, appargs=AppArgs}=Node|_] ->
            Vars = #{
                event => <<"test">>,
                app => App,
                devaddr => DevAddr,
                appargs => AppArgs,
                datetime => calendar:universal_time()},
            lager:debug("Sending connector test ~p to ~p", [App, lorawan_utils:binary_to_hex(DevAddr)]),
            lorawan_backend_factory:event(App, Node, Vars);
        [] ->
            lager:debug("Connector not linked with any node")
    end,
    {true, Req, State};
handle_action(Req, State) ->
    {stop, cowboy_req:reply(405, Req), State}.

resource_exists(Req, #state{app=undefined}=State) ->
    {true, Req, State};
resource_exists(Req, #state{app=App}=State) ->
    case mnesia:dirty_read(handler, App) of
        [#handler{}] ->
            {true, Req, State};
        [] ->
            {false, Req, State}
    end.

% end of file
