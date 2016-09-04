%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin).

-export([handle_authorization/2]).
-export([parse_user/1, build_user/1]).
-export([parse_gateway/1, build_gateway/1]).
-export([parse_device/1, build_device/1]).
-export([parse_link/1, build_link/1]).

-include("lorawan.hrl").

handle_authorization(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User, Pass} ->
            case user_password(User) of
                Pass -> {true, Req, User};
                _ -> {{false, <<"Basic realm=\"lorawan-server\"">>}, Req, State}
            end;
        _ ->
            {{false, <<"Basic realm=\"lorawan-server\"">>}, Req, State}
    end.

user_password(User) ->
    case mnesia:dirty_read(users, User) of
        [] -> undefined;
        [U] -> U#user.pass
    end.

parse_user(List) ->
    ?to_record(user, parse_admin(List)).
build_user(Rec) ->
    build_admin(?to_proplist(user, Rec)).

parse_gateway(List) ->
    ?to_record(gateway, parse_admin(List)).
build_gateway(Rec) ->
    build_admin(?to_proplist(gateway, Rec)).

parse_device(List) ->
    ?to_record(device, parse_admin(List)).
build_device(Rec) ->
    build_admin(?to_proplist(device, Rec)).

parse_link(List) ->
    ?to_record(link, parse_admin(List)).
build_link(Rec) ->
    build_admin(?to_proplist(link, Rec)).

parse_admin(List) ->
    lists:map(
        fun ({Key, null}) -> {Key, undefined};
            ({Key, Value}) when Key == netid -> {Key, lorawan_mac:hex_to_binary(Value)};
            ({Key, Value}) when Key == mac; Key == netid;
                                Key == deveui; Key == appeui; Key == appkey; Key == link;
                                Key == devaddr; Key == nwkskey; Key == appskey -> {Key, lorawan_mac:hex_to_binary(Value)};
            ({Key, Value}) when Key == gpspos -> {Key, parse_latlon(Value)};
            ({Key, Value}) -> {Key, Value}
        end,
        List).

build_admin(List) ->
    lists:map(
        fun ({Key, undefined}) -> {Key, null};
            ({Key, Value}) when Key == netid -> {Key, lorawan_mac:binary_to_hex(Value)};
            ({Key, Value}) when Key == mac; Key == netid;
                                Key == deveui; Key == appeui; Key == appkey; Key == link;
                                Key == devaddr; Key == nwkskey; Key == appskey -> {Key, lorawan_mac:binary_to_hex(Value)};
            ({Key, Value}) when Key == gpspos -> {Key, build_latlon(Value)};
            ({Key, Value}) -> {Key, Value}
        end,
        List).

parse_latlon(List) ->
    {proplists:get_value(lat, List), proplists:get_value(lon, List)}.

build_latlon({Lat, Lon}) ->
    [{lat, Lat}, {lon, Lon}].

% end of file
