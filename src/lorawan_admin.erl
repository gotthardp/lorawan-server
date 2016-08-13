%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin).

-export([handle_authorization/2]).
-export([parse_device/1, build_device/1]).
-export([parse_link/1, build_link/1]).

-include("lorawan.hrl").

handle_authorization(Req, State) ->
    {ok, {User, Pass}} = application:get_env(lorawan_server, http_admin_credentials),
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User, Pass} ->
            {true, Req, User};
        _ ->
            {{false, <<"Basic realm=\"lorawan-server\"">>}, Req, State}
    end.

parse_device(List) ->
    ?to_record(device,
        lists:map(
            fun ({Key, null}) -> {Key, undefined};
                ({Key, Value}) when Key == deveui; Key == appeui; Key == appkey; Key == link -> {Key, lorawan_mac:hex_to_binary(Value)};
                ({Key, Value}) -> {Key, Value}
            end,
            List)).

build_device(Rec) ->
    lists:map(
        fun ({Key, undefined}) -> {Key, null};
            ({Key, Value}) when Key == deveui; Key == appeui; Key == appkey; Key == link -> {Key, lorawan_mac:binary_to_hex(Value)};
            ({Key, Value}) -> {Key, Value}
        end,
        ?to_proplist(device, Rec)).

parse_link(List) ->
    ?to_record(link,
        lists:map(
            fun ({Key, Value}) when Key == devaddr; Key == nwkskey; Key == appskey -> {Key, lorawan_mac:hex_to_binary(Value)};
                ({Key, Value}) -> {Key, Value}
            end,
            List)).

build_link(Rec) ->
    lists:map(
        fun ({Key, Value}) when Key == devaddr; Key == nwkskey; Key == appskey -> {Key, lorawan_mac:binary_to_hex(Value)};
            ({Key, Value}) -> {Key, Value}
        end,
        ?to_proplist(link, Rec)).

% end of file
