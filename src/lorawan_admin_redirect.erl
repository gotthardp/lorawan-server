%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_redirect).

-export([init/2]).

init(Req0, Target) ->
    URI = cowboy_req:uri(Req0, Target),
    Req = cowboy_req:reply(301, #{<<"location">> => URI}, Req0),
    {ok, Req, Target}.

% end of file
