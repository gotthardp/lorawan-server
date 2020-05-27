%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_control).

-export([backup/1, restore/1, stop/0]).

backup(Name) ->
    invoke(mnesia, backup, [Name]).

restore(Name) ->
    invoke(mnesia, restore, [Name, [{default_op, recreate_tables}]]).

stop() ->
    invoke(init, stop, []).

invoke(Module, Fun, Params) ->
    % use short names, so only the first part of the hostname
    [Host | _] = string:lexemes(net_adm:localhost(), "."),
    Node = list_to_atom(string:join(["lorawan", Host], "@")),
    case rpc:call(Node, Module, Fun, Params) of
        ok -> ok;
        {atomic, _} -> ok;
        {badrpc, Reason} ->
            io:format("~w command failed: ~p~n", [Node, Reason])
    end.

% end of file
