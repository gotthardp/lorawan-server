%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(test_admin).

-export([add_gateway/1, add_node/1]).

add_gateway(MAC) ->
    post_json("gateways", [{mac, lorawan_mac:binary_to_hex(MAC)}, {tx_rfch, 0}, {netid, <<"000000">>},
        {gpspos, [{lat, 0}, {lon, 0}]}, {gpsalt, 0}]).

add_node({DevAddr, NwkSKey, AppSKey}) ->
    % set devstat_fcnt so we can test MAC
    post_json("nodes", [{devaddr, lorawan_mac:binary_to_hex(DevAddr)}, {region, <<"EU863-870">>},
        {app, <<"semtech-mote">>}, {nwkskey, NwkSKey}, {appskey, AppSKey},
        {fcntup, 0}, {fcntdown, 0}, {fcnt_check, undefined}, {adr_flag_use, 0},
        {adr_use, [{power, 1}, {datr, 0}, {chans, <<"0-2">>}]},
        {devstat_time, calendar:universal_time()}, {devstat_fcnt, 3}]).

post_json(Uri, Body) ->
    {ok, {{_Version, 204, _ReasonPhrase}, _Headers, _Body}} =
         httpc:request(post, {"http://localhost:8080/" ++ Uri,
             [{"Authorization", "Basic " ++ base64:encode_to_string("admin:admin")}],
             "application/json", jsx:encode(Body)}, [], []).

% end of file
