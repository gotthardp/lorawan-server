%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_connections).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

-export([handle_get/2, handle_action/2]).

-include("lorawan_db.hrl").
-record(state, {app, action, scopes, auth_fields}).

init(Req, Scopes) ->
    App = cowboy_req:binding(app, Req),
    Action = cowboy_req:binding(action, Req),
    {cowboy_rest, Req, #state{app=App, action=Action, scopes=Scopes}}.

allowed_methods(Req, #state{action=undefined}=State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"POST">>], Req, State}.

is_authorized(Req, #state{scopes=Scopes}=State) ->
    case lorawan_admin:handle_authorization(Req, Scopes) of
        {true, AuthFields} ->
            {true, Req, State#state{auth_fields=AuthFields}};
        Else ->
            {Else, Req, State}
    end.

forbidden(Req, #state{auth_fields=AuthFields}=State) ->
    {lorawan_admin:fields_empty(AuthFields), Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, #state{app=undefined}=State) ->
    Filter = lorawan_admin:parse(get_filters(Req)),
    Items =
        lists:filter(
            fun(Conn) ->
                filter_matches(Conn, Filter)
            end,
            get_connections(pg2:which_groups(), [])),
    {jsx:encode(Items), Req, State};
handle_get(Req, #state{app=App}=State) ->
    {jsx:encode(get_connection(App)), Req, State}.

get_filters(Req) ->
    case cowboy_req:match_qs([{'_filters', [], <<"{}">>}], Req) of
        #{'_filters' := Filter} ->
            jsx:decode(Filter, [return_maps, {labels, atom}])
    end.

filter_matches(_Conn, []) ->
    true;
filter_matches(Conn, Filter) ->
    lists:all(
        fun({Key, Value}) ->
            case maps:get(Key, Conn, undefined) of
                Value -> true;
                _ -> false
            end
        end,
        maps:to_list(Filter)).

get_connections([{backend, App}|More], Acc) ->
    get_connections(More, Acc++get_connection(App));
get_connections([_Else|More], Acc) ->
    get_connections(More, Acc);
get_connections([], Acc)->
    Acc.

get_connection(App) ->
    case pg2:get_members({backend, App}) of
        List when is_list(List) ->
            lists:foldl(
                fun(Pid, Acc) ->
                    Acc ++ get_connection0(Pid, App)
                end,
                [], List);
        {error, _} ->
            []
    end.

get_connection0(Pid, App) ->
    Pid ! {status, self()},
    receive
        {status, Data} ->
            Data
    after
        500 ->
            [#{pid => lorawan_connector:pid_to_binary(self()), app => App, status => <<"disconnected">>}]
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
