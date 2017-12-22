%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_http_registry).
-behaviour(gen_server).

-export([start_link/0, update_routes/2, delete_routes/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").
-include("lorawan_application.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_routes(Id, Routes) ->
    gen_server:call(?MODULE, {update_routes, Id, Routes}).

delete_routes(Id) ->
    gen_server:call(?MODULE, {delete_routes, Id}).

init([]) ->
    {ok, Routes} = lorawan_application:init(),
    State = dict:from_list(Routes),
    % setup routes
    Dispatch = compile_routes(State),
    case application:get_env(http_admin_listen, undefined) of
        undefined ->
            ok;
        HttpOpts ->
            {ok, _} = cowboy:start_clear(http, HttpOpts,
                #{env => #{dispatch => Dispatch},
                stream_handlers => [lorawan_admin_logger, cowboy_compress_h, cowboy_stream_h]})
    end,
    case application:get_env(http_admin_listen_ssl, undefined) of
        undefined ->
            ok;
        SslOpts ->
            {ok, _} = cowboy:start_tls(https, SslOpts,
                #{env => #{dispatch => Dispatch},
                stream_handlers => [lorawan_admin_logger, cowboy_compress_h, cowboy_stream_h]})
    end,
    {ok, State}.

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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(http),
    cowboy:stop_listener(https),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_routes(State) ->
    Dispatch = compile_routes(State),
    case application:get_env(http_admin_listen, undefined) of
        undefined ->
            ok;
        _HttpOpts ->
            cowboy:set_env(http, dispatch, Dispatch)
    end,
    case application:get_env(http_admin_listen_ssl, undefined) of
        undefined ->
            ok;
        _SslOpts ->
            cowboy:set_env(https, dispatch, Dispatch)
    end.

compile_routes(Dict) ->
    cowboy_router:compile([
        {'_', get_routes(Dict)}
    ]).

get_routes(Dict) ->
    dict:fold(
        fun(_Key, Routes, Acc) ->
            Acc++Routes
        end,
        static_routes(), Dict).

%% https://ninenines.eu/docs/en/cowboy/2.2/guide/routing/
static_routes() -> [
    {"/servers", lorawan_admin_servers, []},
    {"/applications/[:name]", lorawan_admin_applications, []},
    {"/users/[:name]", lorawan_admin_db_record,
        [users, user, record_info(fields, user)]},
    {"/networks/[:name]", lorawan_admin_db_record,
        [networks, network, record_info(fields, network)]},
    {"/gateways/[:mac]", lorawan_admin_db_record,
        [gateways, gateway, record_info(fields, gateway)]},
    {"/multicast_channels/[:devaddr]", lorawan_admin_db_record,
        [multicast_channels, multicast_channel, record_info(fields, multicast_channel)]},
    {"/profiles/[:name]", lorawan_admin_db_record,
        [profiles, profile, record_info(fields, profile)]},
    {"/devices/[:deveui]", lorawan_admin_db_record,
        [devices, device, record_info(fields, device)]},
    {"/nodes/[:devaddr]", lorawan_admin_db_record,
        [nodes, node, record_info(fields, node)]},
    {"/ignored_nodes/[:devaddr]", lorawan_admin_db_record,
        [ignored_nodes, ignored_node, record_info(fields, ignored_node)]},
    {"/txframes/[:frid]", lorawan_admin_db_record,
        [txframes, txframe, record_info(fields, txframe)]},
    {"/rxframes/[:frid]", lorawan_admin_db_record,
        [rxframes, rxframe, record_info(fields, rxframe)]},
    {"/handlers/[:appid]", lorawan_admin_db_record,
        [handlers, handler, record_info(fields, handler)]},
    {"/connectors/[:connid]", lorawan_admin_db_record,
        [connectors, connector, record_info(fields, connector)]},
    {"/events/[:evid]", lorawan_admin_db_record,
        [events, event, record_info(fields, event)]},
    {"/upload", lorawan_admin_upload, []},
    {"/timeline", lorawan_admin_timeline, []},
    {"/pgraph/:mac", lorawan_admin_gwgraph, [pgraph]},
    {"/tgraph/:mac", lorawan_admin_gwgraph, [tgraph]},
    {"/rgraph/:devaddr", lorawan_admin_rxgraph, [rgraph]},
    {"/qgraph/:devaddr", lorawan_admin_rxgraph, [qgraph]},
    {"/devstat/:devaddr", lorawan_admin_devstat, []},
    {"/", cowboy_static, {priv_file, lorawan_server, "root.html"}},
    {"/favicon.ico", cowboy_static, {priv_file, lorawan_server, "favicon.ico"}},
    {"/admin", cowboy_static, {priv_file, lorawan_server, "admin/index.html"}},
    {"/admin/[...]", cowboy_static, {priv_dir, lorawan_server, "admin"}}].

% end of file
