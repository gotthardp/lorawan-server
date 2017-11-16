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
    {ok, {{_Version, 401, _ReasonPhrase1}, Headers1, _Body1}} =
        httpc:request(post, {"http://localhost:8080/" ++ Uri, [],
            "application/json", jsx:encode(Body)}, [], []),

    WWWAuthenticate = proplists:get_value("www-authenticate", Headers1),
    [{digest, Params}] = cow_http_hd:parse_www_authenticate(list_to_binary(WWWAuthenticate)),
    Realm = proplists:get_value(<<"realm">>, Params),
    Nonce = proplists:get_value(<<"nonce">>, Params),
    Response = lorawan_http_digest:response(<<"POST">>, list_to_binary(Uri), <<>>,
        {<<"admin">>, <<"lorawan-server">>, <<"admin">>}, Nonce),

    {ok, {{_Version, 204, _ReasonPhrase2}, _Headers2, _Body2}} =
        httpc:request(post, {"http://localhost:8080/" ++ Uri,
            [{"Authorization", binary_to_list(
                lorawan_http_digest:header(digest, [{<<"username">>, <<"admin">>},
                    {<<"realm">>, Realm}, {<<"nonce">>, Nonce}, {<<"uri">>, list_to_binary(Uri)},
                    {<<"response">>, Response}]))}],
            "application/json", jsx:encode(Body)}, [], []).

% end of file
