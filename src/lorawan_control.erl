%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_control).

-export([stop/0]).

stop() ->
    invoke(init, stop, []).

invoke(Module, Fun, Params) ->
    % use short names, so only the first part of the hostname
    [Host | _] = string:tokens(net_adm:localhost(), "."),
    Node = list_to_atom(string:join(["lorawan", Host], "@")),
    case rpc:call(Node, Module, Fun, Params) of
        ok -> ok;
        {badrpc, Reason} ->
            io:format("~w command failed: ~p~n", [Node, Reason])
    end.

% end of file
