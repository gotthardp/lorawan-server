%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(test_admin).

-export([add_area/1, add_gateway/2, add_network/1, add_group/2, add_profile/2, add_node/2]).

add_area(AreaName) ->
    post_json("areas", [{name, AreaName}]).

add_gateway(AreaName, MAC) ->
    post_json("gateways", [{mac, lorawan_utils:binary_to_hex(MAC)}, {area, AreaName},
        {tx_rfch, 0}, {gpspos, [{lat, 0}, {lon, 0}]}, {gpsalt, 0}]).

add_network(NetName) ->
    post_json("networks", [{name, NetName}, {netid, <<"000000">>},
        {region, <<"EU868">>}, {tx_codr, <<"4/5">>},
        {join1_delay, 5}, {join2_delay, 6}, {rx1_delay, 1}, {rx2_delay, 2},
        {gw_power, 16}, {max_eirp, 16},
        {rxwin_init, [{rx1_dr_offset, 0}, {rx2_dr, 0}, {rx2_freq, 869.525}]}]).

add_group(NetName, GroupName) ->
    post_json("groups", [{name, GroupName}, {network, NetName}]).

add_profile(GroupName, ProfName) ->
    post_json("profiles", [{name, ProfName}, {group, GroupName}, {app, <<"semtech-mote">>},
        {fcnt_check, undefined}, {adr_mode, 0}]).

add_node(ProfName, {DevAddr, NwkSKey, AppSKey}) ->
    post_json("nodes", [{devaddr, lorawan_utils:binary_to_hex(DevAddr)}, {profile, ProfName},
        {nwkskey, NwkSKey}, {appskey, AppSKey}, {fcntup, 0}, {fcntdown, 0},
        {adr_flag, 0}, {adr_use, [{power, 1}, {datr, 0}, {chans, <<"0-2">>}]},
        {rxwin_use, [{rx1_dr_offset, 0}, {rx2_dr, 0}, {rx2_freq, 869.525}]},
        {devstat_time, calendar:universal_time()}, {devstat_fcnt, 3}]).

post_json(Uri, Body) ->
    {ok, {{_Version, 401, _ReasonPhrase1}, Headers1, _Body1}} =
        httpc:request(post, {"http://localhost:8080/api/" ++ Uri, [],
            "application/json", jsx:encode(Body)}, [], []),

    WWWAuthenticate = proplists:get_value("www-authenticate", Headers1),
    [{digest, Params}] = cow_http_hd:parse_www_authenticate(list_to_binary(WWWAuthenticate)),
    Realm = proplists:get_value(<<"realm">>, Params),
    Nonce = proplists:get_value(<<"nonce">>, Params),
    Response = lorawan_http_digest:response(<<"POST">>, list_to_binary(Uri), <<>>,
        {<<"admin">>, <<"lorawan-server">>, <<"admin">>}, Nonce),

    {ok, {{_Version, 200, _ReasonPhrase2}, _Headers2, _Body2}} =
        httpc:request(post, {"http://localhost:8080/api/" ++ Uri,
            [{"Authorization", binary_to_list(
                lorawan_http_digest:header(digest, [{<<"username">>, <<"admin">>},
                    {<<"realm">>, Realm}, {<<"nonce">>, Nonce}, {<<"uri">>, list_to_binary(Uri)},
                    {<<"response">>, Response}]))}],
            "application/json", jsx:encode(Body)}, [], []).

% end of file
