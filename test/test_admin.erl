%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(test_admin).

-export([add_network/1, add_gateway/2, add_profile/2, add_node/2]).

add_network(NetName) ->
    post_json("networks", [{name, NetName}, {netid, <<"000000">>},
        {region, <<"EU868">>}, {max_eirp, 16}, {tx_powe, 16}]).

add_gateway(NetName, MAC) ->
    post_json("gateways", [{mac, lorawan_utils:binary_to_hex(MAC)}, {network, NetName},
        {tx_rfch, 0}, {gpspos, [{lat, 0}, {lon, 0}]}, {gpsalt, 0}]).

add_profile(NetName, ProfName) ->
    post_json("profiles", [{name, ProfName}, {network, NetName}, {app, <<"semtech-mote">>},
        {fcnt_check, undefined}, {adr_mode, 0}]).

add_node(ProfName, {DevAddr, NwkSKey, AppSKey}) ->
    post_json("nodes", [{devaddr, lorawan_utils:binary_to_hex(DevAddr)}, {profile, ProfName},
        {nwkskey, NwkSKey}, {appskey, AppSKey}, {fcntup, 0}, {fcntdown, 0},
        {adr_flag, 0}, {adr_use, [{power, 1}, {datr, 0}, {chans, <<"0-2">>}]},
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

    {ok, {{_Version, 204, _ReasonPhrase2}, _Headers2, _Body2}} =
        httpc:request(post, {"http://localhost:8080/api/" ++ Uri,
            [{"Authorization", binary_to_list(
                lorawan_http_digest:header(digest, [{<<"username">>, <<"admin">>},
                    {<<"realm">>, Realm}, {<<"nonce">>, Nonce}, {<<"uri">>, list_to_binary(Uri)},
                    {<<"response">>, Response}]))}],
            "application/json", jsx:encode(Body)}, [], []).

% end of file
