%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin).

-export([handle_authorization/2]).
-export([parse/1, build/1]).

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

parse(List) when is_list(List) ->
    lists:map(fun(Item) -> parse(Item) end,
        List);

parse({Key, Value}) when Value == null; Value == undefined ->
    {Key, undefined};
parse({Key, Value}) when Key == netid -> {Key, lorawan_mac:hex_to_binary(Value)};
parse({Key, Value}) when Key == mac; Key == last_mac; Key == netid; Key == mask;
                        Key == deveui; Key == appeui; Key == appkey; Key == link;
                        Key == devaddr; Key == nwkskey; Key == appskey;
                        Key == data; Key == frid ->
    {Key, lorawan_mac:hex_to_binary(Value)};
parse({Key, Value}) when Key == gpspos ->
    {Key, parse_latlon(Value)};
parse({Key, Value}) when Key == adr_use; Key == adr_set ->
    {Key, parse_adr(Value)};
parse({Key, Value}) when Key == rxwin_use; Key == rxwin_set ->
    {Key, parse_rxwin(Value)};
parse({Key, Value}) when Key == rxq; Key == last_rxq ->
    {Key, ?to_record(rxq, parse(Value))};
parse({Key, Value}) when Key == txdata ->
    {Key, ?to_record(txdata, parse(Value))};
parse({Key, Value}) when Key == last_join; Key == last_reset; Key == last_rx;
                        Key == devstat_time; Key == datetime ->
    {Key, iso8601:parse(Value)};
parse({Key, <<"immediately">>}) when Key == time ->
    {Key, immediately};
parse({Key, Value}) when Key == time ->
    {Key, iso8601:parse_exact(Value)};
parse({Key, Value}) when Key == devstat ->
    {Key, parse_devstat(Value)};
parse({Key, Value}) when Key == last_qs ->
    {Key, lists:map(fun(Item) -> parse_qs(Item) end, Value)};
parse({Key, Value}) when Key == average_qs ->
    {Key, parse_qs(Value)};
parse({Key, Value}) when Key == build; Key == parse ->
    {Key, parse_fun(Value)};
parse(Else) ->
    Else.

build(List) when is_list(List) ->
    lists:foldl(
        fun
            % hide very internal fields
            ({Key, _Value}, A) when Key == srvtmst -> A;
            (Item, A) -> [build(Item) | A]
        end, [], List);

build({Key, undefined}) ->
    {Key, null};
build({Key, Value}) when Key == netid ->
    {Key, lorawan_mac:binary_to_hex(Value)};
build({Key, Value}) when Key == mac; Key == last_mac; Key == netid; Key == mask;
                            Key == deveui; Key == appeui; Key == appkey; Key == link;
                            Key == devaddr; Key == nwkskey; Key == appskey;
                            Key == data; Key == frid ->
    {Key, lorawan_mac:binary_to_hex(Value)};
build({Key, Value}) when Key == gpspos ->
    {Key, build_latlon(Value)};
build({Key, Value}) when Key == adr_use; Key == adr_set ->
    {Key, build_adr(Value)};
build({Key, Value}) when Key == rxwin_use; Key == rxwin_set ->
    {Key, build_rxwin(Value)};
build({Key, Value}) when Key == rxq; Key == last_rxq ->
    {Key, build(?to_proplist(rxq, Value))};
build({Key, Value}) when Key == txdata ->
    {Key, build(?to_proplist(txdata, Value))};
build({Key, immediately}) when Key == time ->
    {Key, <<"immediately">>};
build({Key, Value}) when Key == last_join; Key == last_reset; Key == last_rx;
                            Key == devstat_time; Key == time; Key == datetime ->
    {Key, iso8601:format(Value)};
build({Key, Value}) when Key == devstat ->
    {Key, build_devstat(Value)};
build({Key, Value}) when Key == last_qs ->
    {Key, lists:map(fun(Item) -> build_qs(Item) end, Value)};
build({Key, Value}) when Key == average_qs ->
    {Key, build_qs(Value)};
build({Key, Value}) when Key == build; Key == parse ->
    {Key, build_fun(Value)};
build(Else) ->
    Else.

parse_latlon(List) ->
    {proplists:get_value(lat, List), proplists:get_value(lon, List)}.

build_latlon({Lat, Lon}) ->
    [{lat, Lat}, {lon, Lon}].

parse_adr(List) ->
    {proplists:get_value(power, List), proplists:get_value(datr, List),
        case proplists:get_value(chans, List, null) of
            null -> undefined;
            Val -> text_to_intervals(binary_to_list(Val))
        end}.

build_adr({TXPower, DataRate, Chans}) ->
    [{power, TXPower}, {datr, DataRate}, {chans,
        case Chans of
            undefined -> null;
            Val -> list_to_binary(intervals_to_text(Val))
        end}].

parse_rxwin(List) ->
    {proplists:get_value(rx1_dr_offset, List), proplists:get_value(rx2_dr, List), proplists:get_value(rx2_freq, List)}.

build_rxwin({RX1DROffset, RX2DataRate, Frequency}) ->
    [{rx1_dr_offset, RX1DROffset}, {rx2_dr, RX2DataRate}, {rx2_freq, Frequency}].

parse_devstat(List) ->
    {proplists:get_value(battery, List), proplists:get_value(margin, List)}.

build_devstat({Battery, Margin}) ->
    [{battery, Battery}, {margin, Margin}].

parse_qs(List) ->
    {proplists:get_value(rssi, List), proplists:get_value(snr, List)}.

build_qs({RSSI, SNR}) ->
    [{rssi, RSSI}, {snr, SNR}].

parse_fun(Code) ->
    % try to parse the function
    {ok, Ts, _} = erl_scan:string(binary_to_list(Code)),
    {ok, Exprs} = erl_parse:parse_exprs(Ts),
    {value, Fun, _} = erl_eval:exprs(Exprs, []),
    {Code, Fun}.

build_fun({Code, _Fun}) ->
    Code.

intervals_to_text(List) when is_list(List) ->
    lists:flatten(string:join(
        lists:map(
            fun ({A, A}) -> integer_to_list(A);
                ({B, C}) -> [integer_to_list(B), "-", integer_to_list(C)]
            end, List), ", "));
intervals_to_text(_) ->
    % this is for backward compatibility, will be removed in few months
    % I don't think anyone ever used anything else than 7
    "0-2".

text_to_intervals(Text) ->
    lists:map(
        fun (Item) ->
            case string:tokens(Item, "- ") of
                [A] -> {list_to_integer(A), list_to_integer(A)};
                [B, C] -> {list_to_integer(B), list_to_integer(C)}
            end
        end, string:tokens(Text, ";, ")).

-include_lib("eunit/include/eunit.hrl").

bits_test_()-> [
    ?_assertEqual("0", intervals_to_text([{0,0}])),
    ?_assertEqual("0-2", intervals_to_text([{0,2}])),
    ?_assertEqual("0-2, 5-7", intervals_to_text([{0,2},{5,7}])),
    ?_assertEqual("0-7, 64", intervals_to_text([{0,7},{64,64}])),
    ?_assertEqual("0-71", intervals_to_text([{0,71}])),
    ?_assertEqual([{0,0}], text_to_intervals("0")),
    ?_assertEqual([{0,2},{5,7}], text_to_intervals("0-2, 5-7")),
    ?_assertEqual([{0,7},{64,64}], text_to_intervals("0-7,64")), % without space after comma
    ?_assertEqual([{0,7},{64,64}], text_to_intervals("0-7,  64")), % two spaces after comma
    ?_assertEqual([{0,71}], text_to_intervals("0-71"))
].

% end of file
