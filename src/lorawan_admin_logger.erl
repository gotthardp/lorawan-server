%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_logger).
-behaviour(cowboy_stream).

-export([init/3, data/4, info/3, terminate/3, early_error/5]).

-record(state, {next, path, user, peer}).

init(StreamId, Req, Opts) ->
    {Command, Next} = cowboy_stream:init(StreamId, Req, Opts),
    {Command, #state{next=Next, path=path(Req), user=username(Req), peer=cowboy_req:peer(Req)}}.

path(#{path := Path, qs := <<>>}) ->
    Path;
path(#{path := Path, qs := Qs}) ->
    <<Path/binary, "?", Qs/binary>>.

username(Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {digest, Params} ->
            proplists:get_value(<<"username">>, Params, <<>>);
        _ ->
            <<>>
    end.

data(StreamId, IsFin, Data, #state{next=Next0}=State) ->
    {Command, Next} = cowboy_stream:data(StreamId, IsFin, Data, Next0),
    {Command, State#state{next=Next}}.

info(StreamId, Response, #state{next=Next0, path=Path, user=User, peer=Peer}=State) ->
    {Command, Next} = cowboy_stream:info(StreamId, handle_response(Response, Path, User, Peer), Next0),
    {Command, State#state{next=Next}}.

handle_response({response, Status, Headers, Body}, _Path, _User, _Peer)
        when Status div 100 == 2 ->
    {response, Status, add_extra_headers(Headers), Body};
handle_response({headers, Status, Headers}, _Path, _User, _Peer)
        when Status div 100 == 2 ->
    {headers, Status, add_extra_headers(Headers)};

handle_response({response, Status, _Headers, _Body}=Response, Path, User, Peer) ->
    log_error(Status, Path, User, Peer),
    Response;
handle_response({headers, Status, _Headers}=Response, Path, User, Peer) ->
    log_error(Status, Path, User, Peer),
    Response;

handle_response(Else, _Path, _User, _Peer) ->
    Else.

add_extra_headers(Headers) ->
    {ok, Extra} = application:get_env(lorawan_server, http_extra_headers),
    maps:merge(Extra, Headers).

log_error(Status, _Path, _User, _Peer) when Status == 301; Status == 304; Status == 401 ->
    ok;
log_error(Status, Path, User, {IP, _Port}) ->
    lorawan_utils:throw_warning(server, {http_error, {Status, binary_to_list(Path), User, inet:ntoa(IP)}}).

terminate(StreamId, Reason, #state{next=Next0}) ->
    cowboy_stream:terminate(StreamId, Reason, Next0).

early_error(StreamId, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamId, Reason, PartialReq, Resp, Opts).

% end of file
