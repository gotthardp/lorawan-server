%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin).

-export([handle_authorization/2, get_filters/1, paginate/3]).
-export([parse_admin/1]).
-export([parse_user/1, build_user/1]).
-export([parse_gateway/1, build_gateway/1]).
-export([parse_device/1, build_device/1]).
-export([parse_link/1, build_link/1]).
-export([build_txframe/1]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

handle_authorization(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User, Pass} ->
            case user_password(User) of
                Pass -> {true, Req, State};
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

get_filters(Req) ->
    case cowboy_req:match_qs([{'_filters', [], <<"{}">>}], Req) of
        #{'_filters' := Filter} ->
            jsx:decode(Filter, [{labels, atom}])
    end.

paginate(Req, State, List) ->
    case cowboy_req:match_qs([{'_page', [], <<"1">>}, {'_perPage', [], undefined}], Req) of
        #{'_perPage' := undefined} ->
            {jsx:encode(List), Req, State};
        #{'_page' := Page0, '_perPage' := PerPage0} ->
            {Page, PerPage} = {binary_to_integer(Page0), binary_to_integer(PerPage0)},
            Req2 = cowboy_req:set_resp_header(<<"X-Total-Count">>, integer_to_binary(length(List)), Req),
            {jsx:encode(lists:sublist(List, 1+(Page-1)*PerPage, PerPage)), Req2, State}
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

build_txframe(Rec) ->
    build_admin(?to_proplist(txframe, Rec)).

parse_admin(List) ->
    lists:map(
        fun ({Key, null}) -> {Key, undefined};
            ({Key, Value}) when Key == netid -> {Key, lorawan_mac:hex_to_binary(Value)};
            ({Key, Value}) when Key == mac; Key == netid;
                                Key == deveui; Key == appeui; Key == appkey; Key == link;
                                Key == devaddr; Key == nwkskey; Key == appskey -> {Key, lorawan_mac:hex_to_binary(Value)};
            ({Key, Value}) when Key == gpspos -> {Key, parse_latlon(Value)};
            ({Key, Value}) when Key == adr_use; Key == adr_set -> {Key, parse_adr(Value)};
            ({Key, Value}) when Key == txdata -> {Key, ?to_record(txdata, parse_admin(Value))};
            (Else) -> Else
        end,
        List).

build_admin(List) ->
    lists:foldl(
        fun ({Key, undefined}, A) -> [{Key, null} | A];
            ({Key, Value}, A) when Key == netid -> [{Key, lorawan_mac:binary_to_hex(Value)} | A];
            ({Key, Value}, A) when Key == mac; Key == netid;
                                Key == deveui; Key == appeui; Key == appkey; Key == link;
                                Key == devaddr; Key == nwkskey; Key == appskey;
                                Key == data;
                                Key == frid -> [{Key, lorawan_mac:binary_to_hex(Value)} | A];
            ({Key, Value}, A) when Key == gpspos -> [{Key, build_latlon(Value)} | A];
            ({Key, Value}, A) when Key == datetime -> [{Key, build_datetime(Value)} | A];
            ({Key, Value}, A) when Key == adr_use; Key == adr_set -> [{Key, build_adr(Value)} | A];
            ({Key, Value}, A) when Key == txdata -> [{Key, build_admin(?to_proplist(txdata, Value))} | A];
            (Else, A) -> [Else | A]
        end,
        [], List).

parse_latlon(List) ->
    {proplists:get_value(lat, List), proplists:get_value(lon, List)}.

build_latlon({Lat, Lon}) ->
    [{lat, Lat}, {lon, Lon}].

build_datetime(DateTime) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:gregorian_seconds_to_datetime(DateTime),
    list_to_binary(lists:flatten(io_lib:fwrite("~b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b",
        [Year, Month, Day, Hour, Minute, Second]))).

parse_adr(List) ->
    {proplists:get_value(power, List), proplists:get_value(datr, List),
        case proplists:get_value(chans, List) of
            undefined -> undefined;
            Val -> binary_to_integer(Val, 2)
        end}.

build_adr({TXPower, DataRate, Chans}) ->
    [{power, TXPower}, {datr, DataRate}, {chans,
        case Chans of
            undefined -> undefined;
            Val -> integer_to_binary(Val, 2)
        end}].

% end of file
