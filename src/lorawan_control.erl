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
    Node = list_to_atom(string:join(["lorawan", net_adm:localhost()], "@")),
    case rpc:call(Node, Module, Fun, Params) of
        ok -> ok;
        {badrpc, Reason} ->
            io:format("~w wommand failed: ~w~n", [Node, Reason])
    end.

% end of file
