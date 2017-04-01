%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector_mqtt).
-behaviour(gen_server).

-export([start_link/2, publish/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lorawan.hrl").

-record(state, {mqttc}).

start_link(ConnUri, Conn) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [ConnUri, Conn], []).

publish(Server, Topic, Payload) ->
    gen_server:cast(Server, {publish, Topic, Payload}).

init([ConnUri, Conn]) ->
    {_Scheme, _UserInfo, HostName, Port, _Path, _Query} = ConnUri,
    {ok, C} = emqttc:start_link(lists:append([
        [{host, HostName},
        {port, Port},
        {logger, warning},
        {keepalive, 0}],
        auth_args(ConnUri, Conn),
        ssl_args(ConnUri, Conn)
    ])),
    {ok, #state{mqttc = C}}.

% Microsoft Shared Access Signature
auth_args({_Scheme, _UserInfo, HostName, _Port, _Path, _Query},
        #connector{auth= <<"sas">>, client_id=DeviceID, name=KeyName, pass=SharedKey}) ->
    UserName = lists:flatten(
        io_lib:format("~s/~s/api-version=2016-11-14", [HostName, DeviceID])),
    [{client_id, DeviceID},
    {username, list_to_binary(UserName)},
    {password, list_to_binary(shared_access_token(HostName, DeviceID, KeyName, SharedKey))}];
% normal (and default)
auth_args(_ConnUri, #connector{client_id=ClientId, name=UserName, pass=Password}) ->
    [{client_id, ClientId},
    {username, UserName},
    {password, Password}].

ssl_args({mqtt, _UserInfo, _Host, _Port, _Path, _Query}, _Conn) ->
    [];
ssl_args({mqtts, _UserInfo, _Host, _Port, _Path, _Query}, #connector{certfile=CertFile, keyfile=KeyFile}) ->
    [{ssl, [
        {versions, ['tlsv1.2']},
        {certfile, CertFile}, {keyfile, KeyFile}
    ]}].

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast({publish, Topic, Payload}, State = #state{mqttc = C}) ->
    emqttc:publish(C, Topic, Payload),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({publish, Topic, Payload}, State) ->
    io:format("Message from ~s: ~p~n", [Topic, Payload]),
    {noreply, State};
handle_info({mqttc, C, connected}, State = #state{mqttc = C}) ->
    {noreply, State};
handle_info({mqttc, C, disconnected}, State = #state{mqttc = C}) ->
    {noreply, State};
handle_info(_Unknown, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% Shared Access Signature functions
% see https://docs.microsoft.com/en-us/azure/storage/storage-dotnet-shared-access-signature-part-1

shared_access_token(HostName, DeviceID, undefined, AccessKey) ->
    Res = lists:flatten(
        io_lib:format("~s/devices/~s", [HostName, DeviceID])),
    lists:flatten(
        build_access_token(Res, AccessKey));

shared_access_token(HostName, _DeviceID, KeyName, AccessKey) ->
    Res = lists:flatten(
        io_lib:format("~s/devices", [HostName])),
    lists:flatten(
        [build_access_token(Res, AccessKey), io_lib:format("&skn=~s", [KeyName])]).

build_access_token(Res0, AccessKey) ->
    build_access_token(Res0, AccessKey, 60*60*24*7). % expires in a week

build_access_token(Res0, AccessKey, Expiry) ->
    Res = http_uri:encode(Res0),
    % seconds since the UNIX epoch
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
     - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    ToSign = lists:flatten(
        io_lib:format("~s~n~B", [Res, Now+Expiry])),
    Sig = http_uri:encode(base64:encode_to_string(
        crypto:hmac(sha256, base64:decode(AccessKey), ToSign))),
    io_lib:format("SharedAccessSignature sr=~s&sig=~s&se=~B", [Res, Sig, Now+Expiry]).

% end of file
