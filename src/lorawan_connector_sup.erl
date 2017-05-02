%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1, delete_child/1]).
-export([init/1]).

-include("lorawan.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Conn) ->
    case parse_uri(Conn#connector.uri) of
        {ok, {mqtt, _, _, _, _, _} = ConnUri} ->
            connect(lorawan_connector_mqtt, ConnUri, Conn);
        {ok, {mqtts, _, _, _, _, _} = ConnUri} ->
            connect(lorawan_connector_mqtt, ConnUri, Conn);
        {ok, _Else} ->
            {error, not_supported};
        {error, Error} ->
            {error, Error}
    end.

connect(Module, ConnUri, Conn) ->
    supervisor:start_child(?SERVER, {Conn#connector.connid,
        {Module, start_link, [ConnUri, Conn]},
        transient, 5000, worker, [Module]}).

delete_child(ConnId) ->
    supervisor:delete_child(?SERVER, ConnId).

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

parse_uri(Uri) when is_binary(Uri) ->
    parse_uri(binary_to_list(Uri));

parse_uri(Uri) when is_list(Uri) ->
    http_uri:parse(Uri, [{scheme_defaults, [{mqtt, 1883}, {mqtts, 8883}]}]).

% end of file
