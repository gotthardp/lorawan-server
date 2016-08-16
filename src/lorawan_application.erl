%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application).

-export([list_all/0]).
-export([init/0, handle/5]).

% update this list to add/remove applications
list_all() ->
    [{<<"loramote">>, lorawan_application_mote}].

init() ->
    do_init(list_all()).

do_init([]) -> ok;
do_init([{App, Module}|Rest]) ->
    case apply(Module, init, [App]) of
        ok -> do_init(Rest);
        Else -> Else
    end.

handle(DevAddr, App, AppID, Port, Data) ->
    case proplists:get_value(App, list_all()) of
        undefined ->
            {error, {unknown_app, App}};
        Module ->
            apply(Module, handle, [DevAddr, App, AppID, Port, Data])
    end.

% end of file
