%
% Copyright (c) 2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_logger).
-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-record(state, {next, path}).

init(StreamId, Req, Opts) ->
    {Command, Next} = cowboy_stream:init(StreamId, Req, Opts),
    {Command, #state{next=Next, path=path(Req)}}.

path(#{path := Path, qs := <<>>}) ->
    Path;
path(#{path := Path, qs := Qs}) ->
    <<Path/binary, "?", Qs/binary>>.

data(StreamId, IsFin, Data, #state{next=Next0}=State) ->
    {Command, Next} = cowboy_stream:data(StreamId, IsFin, Data, Next0),
    {Command, State#state{next=Next}}.

info(StreamId, Response, #state{next=Next0, path=Path}=State) ->
    {Command, Next} = cowboy_stream:info(StreamId, handle_response(Response, Path), Next0),
    {Command, State#state{next=Next}}.

handle_response({response, Status, Headers, Body}, _Path)
        when Status div 100 == 2 ->
    {response, Status, add_security_headers(Headers), Body};
handle_response({headers, Status, Headers}, _Path)
        when Status div 100 == 2 ->
    {headers, Status, add_security_headers(Headers)};

handle_response({response, Status, _Headers, _Body}=Response, Path) ->
    log_error(Status, Path),
    Response;
handle_response({headers, Status, _Headers}=Response, Path) ->
    log_error(Status, Path),
    Response;

handle_response(Else, _Path) ->
    Else.

add_security_headers(Headers) ->
    {ok, ContentSecurity} = application:get_env(lorawan_server, http_content_security),
    Headers#{
        <<"content-security-policy">> => ContentSecurity
    }.

log_error(Status, _Path) when Status == 304; Status == 401 ->
    ok;
log_error(Status, Path) ->
    lorawan_utils:throw_warning(server, {http_error, {Status, binary_to_list(Path)}}).

terminate(StreamId, Reason, #state{next=Next0}) ->
    cowboy_stream:terminate(StreamId, Reason, Next0).

early_error(StreamId, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamId, Reason, PartialReq, Resp, Opts).

% end of file
