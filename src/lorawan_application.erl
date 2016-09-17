%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application).

-export([init/0, handle/5]).

init() ->
    {ok, Modules} = application:get_env(plugins),
    do_init(Modules, []).

do_init([], Acc) -> {ok, Acc};
do_init([{App, Module}|Rest], Acc) ->
    case apply(Module, init, [App]) of
        ok -> do_init(Rest, Acc);
        {ok, Handlers} -> do_init(Rest, Acc++Handlers);
        Else -> Else
    end.

handle(DevAddr, App, AppID, Port, Data) ->
    {ok, Modules} = application:get_env(plugins),
    case proplists:get_value(App, Modules) of
        undefined ->
            {error, {unknown_app, App}};
        Module ->
            apply(Module, handle, [DevAddr, App, AppID, Port, Data])
    end.

% end of file
