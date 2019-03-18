%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_http_registry).
-behaviour(gen_server).

-export([start_link/0, update/2, delete/1, get/1, get_static/1, get_custom/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update(Id, Data) ->
    gen_server:call(?MODULE, {update, Id, Data}).

delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

get(Type) ->
    gen_server:call(?MODULE, {get, Type}).

init([]) ->
    self() ! initialize,
    {ok, dict:new()}.

handle_call({update, Id, Data}, _From, State) ->
    State2 = dict:store(Id, Data, State),
    update_routes(State2),
    {reply, ok, State2};
handle_call({delete, Id}, _From, State) ->
    State2 = dict:erase(Id, State),
    update_routes(State2),
    {reply, ok, State2};
handle_call({get, Type}, _From, State) ->
    Res = get_static(Type)++join_entries(Type, State)++get_custom(Type),
    {reply, Res, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(initialize, _State) ->
    {ok, Data} = lorawan_application:init(),
    State2 = dict:from_list(Data),
    update_routes(State2),
    {noreply, State2};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_routes(State) ->
    Routes = join_entries(routes, State),
    lager:debug("New routes ~p", [Routes]),
    Dispatch =
        cowboy_router:compile([
            % static routes take precedence
            {'_', get_static(routes)++Routes++get_custom(routes)}
        ]),
    Listen = ranch:info(),
    case {proplists:is_defined(http, Listen), proplists:is_defined(https, Listen)} of
        {false, false} ->
            ok;
        {true, false} ->
            cowboy:set_env(http, dispatch, Dispatch);
        {false, true} ->
            cowboy:set_env(https, dispatch, Dispatch);
        {true, true} ->
            cowboy:set_env(https, dispatch, Dispatch),
            case application:get_env(lorawan_server, http_admin_redirect_ssl, true) of
                false ->
                    cowboy:set_env(http, dispatch, Dispatch);
                true ->
                    ok
            end
    end.

join_entries(Key, Dict) ->
    dict:fold(
        fun(_Key, Data, Acc) ->
            Acc ++ maps:get(Key, Data, [])
        end,
        [], Dict).

get_static(scopes) ->
    [<<"unlimited">>, <<"web-admin">>,
    <<"server:read">>, <<"server:write">>,
    <<"network:read">>, <<"network:write">>,
    <<"device:read">>, <<"device:write">>, <<"device:send">>,
    <<"backend:read">>, <<"backend:write">>];
%% https://ninenines.eu/docs/en/cowboy/2.2/guide/routing/
get_static(routes) ->
    [{"/api/scopes/[:name]", lorawan_admin_scopes,
        [{<<"server:read">>, '*'}]},
    {"/api/config/[:name]", lorawan_admin_db_record,
        {config, record_info(fields, config),
            {[{<<"server:read">>, '*'}], [{<<"server:write">>, '*'}]}}},
    {"/api/servers/[:sname]", lorawan_admin_servers,
        {[{<<"server:read">>, '*'}], [{<<"server:write">>, '*'}]}},
    {"/api/applications/[:name]", lorawan_admin_applications,
        [{<<"web-admin">>, '*'}]},
    {"/api/users/[:name]", lorawan_admin_db_record,
        {user, record_info(fields, user),
            {[{<<"server:read">>, '*'}], [{<<"server:write">>, '*'}]}}},
    {"/api/areas/[:name]", lorawan_admin_db_record,
        {area, record_info(fields, area),
            {[{<<"network:read">>, '*'}], [{<<"network:write">>, '*'}]}}},
    {"/api/gateways/[:mac]", lorawan_admin_db_record,
        {gateway, record_info(fields, gateway),
            {[{<<"network:read">>, '*'}],
            % the server-written fields shall be read-only
            [{<<"network:write">>, [mac, area, tx_rfch, ant_gain, desc, gpspos, gpsalt]}]}}},
    {"/api/multicast_channels/[:devaddr]", lorawan_admin_db_record,
        {multicast_channel, record_info(fields, multicast_channel),
            {[{<<"network:read">>, '*'}], [{<<"network:write">>, '*'}]}}},
    {"/api/networks/[:name]", lorawan_admin_db_record,
        {network, record_info(fields, network),
            {[{<<"network:read">>, '*'}], [{<<"network:write">>, '*'}]}}},
    {"/api/groups/[:name]", lorawan_admin_db_record,
        {group, record_info(fields, group),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/profiles/[:name]", lorawan_admin_db_record,
        {profile, record_info(fields, profile),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/choices/regions", lorawan_admin_choices,
        {regions, [{<<"web-admin">>, '*'}]}},
    {"/api/choices/groups", lorawan_admin_choices,
        {groups, [{<<"web-admin">>, '*'}]}},
    {"/api/choices/profiles", lorawan_admin_choices,
        {profiles, [{<<"web-admin">>, '*'}]}},
    {"/api/devices/[:deveui]", lorawan_admin_db_record,
        {device, record_info(fields, device),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/devices/:deveui/:field", lorawan_admin_db_field,
        {device, record_info(fields, device),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/nodes/[:devaddr]", lorawan_admin_db_record,
        {node, record_info(fields, node),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/nodes/:devaddr/:field", lorawan_admin_db_field,
        {node, record_info(fields, node),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/ignored_nodes/[:devaddr]", lorawan_admin_db_record,
        {ignored_node, record_info(fields, ignored_node),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/queued/[:frid]", lorawan_admin_db_record,
        {queued, record_info(fields, queued),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/rxframes/[:frid]", lorawan_admin_db_record,
        {rxframe, record_info(fields, rxframe),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/handlers/[:app]", lorawan_admin_db_record,
        {handler, record_info(fields, handler),
            {[{<<"backend:read">>, '*'}], [{<<"backend:write">>, '*'}]}}},
    {"/api/connectors/[:connid]", lorawan_admin_db_record,
        {connector, record_info(fields, connector),
            {[{<<"backend:read">>, '*'}], [{<<"backend:write">>, '*'}]}}},
    {"/api/connections/[:app]", lorawan_admin_connections,
        [{<<"backend:read">>, '*'}]},
    {"/api/connections/:app/:action", lorawan_admin_connections,
        {[{<<"backend:read">>, '*'}], [{<<"backend:write">>, '*'}]}},
    {"/api/events/[:evid]", lorawan_admin_db_record,
        {event, record_info(fields, event),
            {[{<<"device:read">>, '*'}], [{<<"device:write">>, '*'}]}}},
    {"/api/upload", lorawan_admin_upload,
        {[], [{<<"backend:write">>, '*'}]}},
    {"/admin", lorawan_admin_static,
        {priv_file, lorawan_server, <<"admin/index.html">>,
            [{<<"web-admin">>, '*'}]}},
    {"/admin/timeline", lorawan_admin_timeline,
        [{<<"web-admin">>, '*'}]},
    {"/admin/admin-config.js", lorawan_admin_config_js,
        [{<<"web-admin">>, '*'}]},
    {"/admin/sgraph/:sname", lorawan_admin_graph_server,
        [{<<"server:read">>, '*'}]},
    {"/admin/pgraph/:mac", lorawan_admin_graph_gw,
        {pgraph, [{<<"network:read">>, '*'}]}},
    {"/admin/tgraph/:mac", lorawan_admin_graph_gw,
        {tgraph, [{<<"network:read">>, '*'}]}},
    {"/admin/rgraph/:devaddr", lorawan_admin_graph_rx,
        {rgraph, [{<<"device:read">>, '*'}]}},
    {"/admin/qgraph/:devaddr", lorawan_admin_graph_rx,
        {qgraph, [{<<"device:read">>, '*'}]}},
    {"/admin/ngraph/:devaddr", lorawan_admin_graph_node,
        [{<<"device:read">>, '*'}]},
    {"/admin/[...]", lorawan_admin_static,
        {priv_dir, lorawan_server, <<"admin">>,
            [{<<"web-admin">>, '*'}]}}].

get_custom(scopes) ->
    [];
get_custom(routes) ->
    % serve custom web-pages
    custom_web(application:get_env(lorawan_server, http_custom_web, [])).

custom_web([{URL, dir, Path, Scope} | Dirs]) ->
    [{URL, lorawan_admin_static,
        {dir, Path, Scope}}
    | custom_web(Dirs)];
custom_web([{URL, file, Path, Scope} | Dirs]) ->
    [{URL, lorawan_admin_static,
        {file, Path, Scope}}
    | custom_web(Dirs)];
custom_web([]) ->
    AdminPath = application:get_env(lorawan_server, http_admin_path, <<"/admin">>),
    % default icon
    [{"/favicon.ico", lorawan_admin_static,
        {priv_file, lorawan_server, <<"favicon.ico">>,
            % anyone, even a REST API may request favicon
            [{'*', '*'}]}},
    % last-chance redirection
    {"/", lorawan_admin_redirect, #{path => AdminPath}}].

% end of file
