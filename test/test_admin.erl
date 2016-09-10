%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(test_admin).

-export([add_device/1, add_link/3]).

add_device(MAC) ->
    post_json("gateways", [{mac, MAC}, {netid, <<"000000">>}, {gpspos, [{lat, 0}, {lon, 0}]}, {gpsalt, 0}]).

add_link(DevAddr, NwkSKey, AppSKey) ->
    post_json("links", [{devaddr, DevAddr}, {app, <<"semtech-mote">>}, {nwkskey, NwkSKey}, {appskey, AppSKey}, {fcntup, 0}, {fcntdown, 0}]).

post_json(Uri, Body) ->
    {ok, {{_Version, 204, _ReasonPhrase}, _Headers, _Body}} =
         httpc:request(post, {"http://localhost:8080/" ++ Uri,
             [{"Authorization", "Basic " ++ base64:encode_to_string("admin:admin")}],
             "application/json", jsx:encode(Body)}, [], []).

% end of file
