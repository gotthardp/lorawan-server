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
    case Response of
        {response, Status, _Headers, _Body} -> log_error(Status, Path);
        {headers, Status, _Headers} -> log_error(Status, Path);
        _Else -> ok
    end,
    {Command, Next} = cowboy_stream:info(StreamId, Response, Next0),
    {Command, State#state{next=Next}}.

log_error(Status, _Path) when Status div 100 == 2; Status == 304; Status == 401 ->
    ok;
log_error(Status, Path) ->
    lager:error("HTTP ~B ~s", [Status, Path]).

terminate(StreamId, Reason, #state{next=Next0}) ->
    cowboy_stream:terminate(StreamId, Reason, Next0).

early_error(StreamId, Reason, PartialReq, Resp, Opts) ->
    cowboy_stream:early_error(StreamId, Reason, PartialReq, Resp, Opts).

% end of file
