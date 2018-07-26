%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_http_registry).
-behaviour(gen_server).

-export([start_link/0, static_routes/0, update_routes/2, delete_routes/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_routes(Id, Routes) ->
    gen_server:call(?MODULE, {update_routes, Id, Routes}).

delete_routes(Id) ->
    gen_server:call(?MODULE, {delete_routes, Id}).

init([]) ->
    self() ! initialize,
    {ok, dict:new()}.

handle_call({update_routes, Id, Routes}, _From, State) ->
    State2 = dict:store(Id, Routes, State),
    update_routes(State2),
    {reply, ok, State2};
handle_call({delete_routes, Id}, _From, State) ->
    State2 = dict:erase(Id, State),
    update_routes(State2),
    {reply, ok, State2}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(initialize, _State) ->
    {ok, Routes} = lorawan_application:init(),
    State2 = dict:from_list(Routes),
    update_routes(State2),
    {noreply, State2};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_routes(State) ->
    Dispatch = compile_routes(State),
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

compile_routes(Dict) ->
    Routes = get_routes(Dict),
    lager:debug("New routes ~p", [Routes]),
    cowboy_router:compile([
        % static routes take precedence
        {'_', static_routes()++Routes}
    ]).

get_routes(Dict) ->
    dict:fold(
        fun(_Key, Routes, Acc) ->
            Acc++Routes
        end,
        [], Dict).

%% https://ninenines.eu/docs/en/cowboy/2.2/guide/routing/
static_routes() ->
    AdminPath = application:get_env(lorawan_server, http_admin_path, <<"/admin">>),
    [{"/api/config/[:name]", lorawan_admin_db_record,
        [config, record_info(fields, config)]},
    {"/api/servers/[:sname]", lorawan_admin_servers, []},
    {"/api/applications/[:name]", lorawan_admin_applications, []},
    {"/api/users/[:name]", lorawan_admin_db_record,
        [user, record_info(fields, user)]},
    {"/api/areas/[:name]", lorawan_admin_db_record,
        [area, record_info(fields, area)]},
    {"/api/gateways/[:mac]", lorawan_admin_db_record,
        [gateway, record_info(fields, gateway)]},
    {"/api/multicast_channels/[:devaddr]", lorawan_admin_db_record,
        [multicast_channel, record_info(fields, multicast_channel)]},
    {"/api/networks/[:name]", lorawan_admin_db_record,
        [network, record_info(fields, network)]},
    {"/api/groups/[:name]", lorawan_admin_db_record,
        [group, record_info(fields, group)]},
    {"/api/profiles/[:name]", lorawan_admin_db_record,
        [profile, record_info(fields, profile)]},
    {"/api/choices/regions", lorawan_admin_choices, regions},
    {"/api/choices/networks", lorawan_admin_choices, networks},
    {"/api/choices/profiles", lorawan_admin_choices, profiles},
    {"/api/devices/[:deveui]", lorawan_admin_db_record,
        [device, record_info(fields, device)]},
    {"/api/nodes/[:devaddr]", lorawan_admin_db_record,
        [node, record_info(fields, node)]},
    {"/api/ignored_nodes/[:devaddr]", lorawan_admin_db_record,
        [ignored_node, record_info(fields, ignored_node)]},
    {"/api/queued/[:frid]", lorawan_admin_db_record,
        [queued, record_info(fields, queued)]},
    {"/api/rxframes/[:frid]", lorawan_admin_db_record,
        [rxframe, record_info(fields, rxframe)]},
    {"/api/handlers/[:app]", lorawan_admin_db_record,
        [handler, record_info(fields, handler)]},
    {"/api/connectors/[:connid]", lorawan_admin_db_record,
        [connector, record_info(fields, connector)]},
    {"/api/connections/[:app]", lorawan_admin_connections, []},
    {"/api/connections/:app/:action", lorawan_admin_connections, []},
    {"/api/events/[:evid]", lorawan_admin_db_record,
        [event, record_info(fields, event)]},
    {"/api/upload", lorawan_admin_upload, []},
    {"/admin", lorawan_admin_static, {priv_file, lorawan_server, <<"admin/index.html">>}},
    {"/admin/timeline", lorawan_admin_timeline, []},
    {"/admin/admin-config.js", lorawan_admin_config_js, []},
    {"/admin/sgraph/:sname", lorawan_admin_graph_server, []},
    {"/admin/pgraph/:mac", lorawan_admin_graph_gw, [pgraph]},
    {"/admin/tgraph/:mac", lorawan_admin_graph_gw, [tgraph]},
    {"/admin/rgraph/:devaddr", lorawan_admin_graph_rx, [rgraph]},
    {"/admin/qgraph/:devaddr", lorawan_admin_graph_rx, [qgraph]},
    {"/admin/ngraph/:devaddr", lorawan_admin_graph_node, []},
    {"/admin/[...]", lorawan_admin_static, {priv_dir, lorawan_server, <<"admin">>}},
    {"/", lorawan_admin_redirect, #{path => AdminPath}},
    {"/favicon.ico", lorawan_admin_static, {priv_file, lorawan_server, <<"favicon.ico">>}}].

% end of file
