%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin).

-export([handle_authorization/2]).
-export([check_health/1, check_health/3, parse/1, build/1]).
-export([parse_field/2, build_field/2]).

-export([check_reception/1]).
-export([check_reset/1, check_battery/1, check_margin/1, check_adr/1]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

-define(REALM, <<"lorawan-server">>).

handle_authorization(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {digest, Params} ->
            Method = cowboy_req:method(Req),
            UserName = proplists:get_value(<<"username">>, Params, <<>>),
            Realm = proplists:get_value(<<"realm">>, Params, <<>>),
            Nonce = proplists:get_value(<<"nonce">>, Params, <<>>),
            URI = proplists:get_value(<<"uri">>, Params, <<>>),
            Response = proplists:get_value(<<"response">>, Params, <<>>),
            % retrieve and check password
            case get_password_hash(<<"admin">>, UserName, Realm) of
                undefined ->
                    {{false, digest_header()}, Req, State};
                HA1 ->
                    case lorawan_http_digest:response(Method, URI, <<>>, HA1, Nonce) of
                        Response ->
                            {true, Req, State};
                        _Else ->
                            {{false, digest_header()}, Req, State}
                    end
            end;
        _Else ->
            {{false, digest_header()}, Req, State}
    end.

digest_header() ->
    Nonce = lorawan_http_digest:nonce(16),
    lorawan_http_digest:header(digest, [
        {<<"realm">>, ?REALM}, {<<"nonce">>, Nonce}]).

get_password_hash(Role, UserName, Realm) ->
    case mnesia:dirty_read(users, UserName) of
        [#user{pass=Pass, roles=undefined}] ->
            % temporary provisions for backward compatibility
            lorawan_http_digest:ha1({UserName, Realm, Pass});
        [#user{pass=Pass, roles=Roles}] ->
            case lists:member(Role, Roles) of
                true ->
                    lorawan_http_digest:ha1({UserName, Realm, Pass});
                false ->
                    undefined
            end;
        _Else ->
            undefined
    end.

check_health(#gateway{} = Gateway) ->
    check_health(Gateway, ?MODULE, [check_reception]);
check_health(#link{} = Link) ->
    check_health(Link, ?MODULE, [check_reset, check_battery, check_margin, check_adr]);
check_health(_Other) ->
    undefined.

check_health(Rec, Module, Funs) ->
    lists:foldl(
        fun
            (_Fun, undefined) ->
                undefined;
            (Fun, {Decay, Alerts}) ->
                case apply(Module, Fun, [Rec]) of
                    {DecayInc, AlertInc} ->
                        {Decay + DecayInc, [AlertInc | Alerts]};
                    ok ->
                        {Decay, Alerts};
                    undefined ->
                        undefined
                end
        end,
        {0, []}, Funs).

check_reception(#gateway{last_rx=undefined}) ->
    {100, disconnected};
check_reception(#gateway{last_rx=LastRx}) ->
    case calendar:datetime_to_gregorian_seconds(calendar:universal_time()) -
            calendar:datetime_to_gregorian_seconds(LastRx) of
        Silent when Silent > 20 ->
            {Silent div 20, disconnected};
        _Else ->
            ok
    end.

check_reset(#link{last_reset=LastRes, reset_count=Count, last_rx=LastRx})
        when LastRes /= undefined, is_number(Count), Count > 1, (LastRx == undefined orelse LastRes > LastRx) ->
    {20*(Count-1), many_resets};
check_reset(#link{}) ->
    ok.

check_battery(#link{devstat=[{_Time, Battery, _Margin, _MaxSNR}|_]}) ->
    if
        Battery == 0 ->
            % connected to external power
            ok;
        Battery < 50 ->
            % TODO: should estimate trend instead
            {100-2*Battery, battery_low};
        Battery == 255 ->
            {25, cannot_measure_battery};
        true ->
            ok
    end;
check_battery(#link{}) ->
    undefined.

check_margin(#link{devstat=[{_Time, _Battery, Margin, MaxSNR}|_]}) ->
    if
        Margin =< MaxSNR+10 ->
            {5*(Margin-MaxSNR), downlink_noise};
        true ->
            ok
    end;
check_margin(#link{}) ->
    undefined.

check_adr(#link{last_rx=undefined}) ->
    % no frame arrived yet, so we don't know the #link.adr_flag_use
    undefined;
check_adr(#link{adr_flag_set=0}) ->
    % disabled, so we don't care
    undefined;
check_adr(#link{adr_flag_use=1, adr_flag_set=Flag, adr_set={TxPower, DataRate, Chans}})
        when Flag >= 1, is_integer(TxPower), is_integer(DataRate), is_list(Chans) ->
    % everything is correctly configured
    ok;
check_adr(#link{adr_flag_use=0, adr_flag_set=Flag}) when Flag >= 1 ->
    {25, adr_not_supported};
check_adr(#link{adr_flag_use=1, adr_flag_set=Flag}) when Flag >= 1 ->
    {25, adr_misconfigured}.

parse(Object) when is_map(Object) ->
    maps:map(fun(Key, Value) -> parse_field(Key, Value) end,
        Object).

build(Object) when is_map(Object) ->
    maps:map(
        fun(Key, Value) -> build_field(Key, Value) end,
        maps:filter(
            fun
                (_Key, undefined) -> false;
                % hide very internal fields
                (Key, _Value) when Key == srvtmst -> false;
                (_, _) -> true
            end, Object)).

parse_field(_Key, Value) when Value == null; Value == undefined ->
    undefined;
parse_field(Key, Value) when Key == mac; Key == last_mac; Key == netid; Key == mask;
                        Key == deveui; Key == appeui; Key == appkey; Key == link;
                        Key == devaddr; Key == nwkskey; Key == appskey;
                        Key == data; Key == frid; Key == evid; Key == eid ->
    lorawan_mac:hex_to_binary(Value);
parse_field(Key, Value) when Key == subid ->
    parse_bitstring(Value);
parse_field(Key, Value) when Key == severity; Key == entity ->
    binary_to_existing_atom(Value, latin1);
parse_field(Key, Value) when Key == gpspos ->
    parse_latlon(Value);
parse_field(Key, Value) when Key == adr_use; Key == adr_set ->
    parse_adr(Value);
parse_field(Key, Value) when Key == rxwin_use; Key == rxwin_set ->
    parse_rxwin(Value);
parse_field(Key, Value) when Key == rxq; Key == last_rxq ->
    ?to_record(rxq, parse(Value));
parse_field(Key, Value) when Key == txdata ->
    ?to_record(txdata, parse(Value));
parse_field(Key, Value) when Key == last_join; Key == first_reset; Key == last_reset;
                        Key == datetime; Key == devstat_time;
                        Key == first_rx; Key == last_rx ->
    iso8601:parse(Value);
parse_field(Key, <<"immediately">>) when Key == time ->
    immediately;
parse_field(Key, Value) when Key == time ->
    iso8601:parse_exact(Value);
parse_field(Key, Value) when Key == devstat ->
    parse_devstat(Value);
parse_field(Key, Value) when Key == dwell ->
    lists:map(
        fun(#{time:=Time, freq:=Freq, duration:=Duration, hoursum:=Sum}) ->
            {iso8601:parse_exact(Time), {Freq, Duration, Sum}}
        end, Value);
parse_field(Key, Value) when Key == delays ->
    lists:map(
        fun(#{date:=Date, srvdelay:=SDelay, nwkdelay:=NDelay}) ->
            {iso8601:parse(Date), SDelay, NDelay}
        end, Value);
parse_field(Key, Value) when Key == last_qs ->
    lists:map(fun(Item) -> parse_qs(Item) end, Value);
parse_field(Key, Value) when Key == average_qs ->
    parse_qs(Value);
parse_field(Key, Value) when Key == build; Key == parse ->
    parse_fun(Value);
parse_field(_Key, Value) ->
    Value.

build_field(_Key, undefined) ->
    null;
build_field(Key, Value) when Key == mac; Key == last_mac; Key == netid; Key == mask;
                        Key == deveui; Key == appeui; Key == appkey; Key == link;
                        Key == devaddr; Key == nwkskey; Key == appskey;
                        Key == data; Key == frid; Key == evid; Key == eid ->
    lorawan_mac:binary_to_hex(Value);
build_field(Key, Value) when Key == subid ->
    build_bitstring(Value);
build_field(Key, Value) when Key == severity; Key == entity ->
    atom_to_binary(Value, latin1);
build_field(Key, Value) when Key == gpspos ->
    build_latlon(Value);
build_field(Key, Value) when Key == adr_use; Key == adr_set ->
    build_adr(Value);
build_field(Key, Value) when Key == rxwin_use; Key == rxwin_set ->
    build_rxwin(Value);
build_field(Key, Value) when Key == rxq; Key == last_rxq ->
    build(?to_map(rxq, Value));
build_field(Key, Value) when Key == txdata ->
    build(?to_map(txdata, Value));
build_field(Key, immediately) when Key == time ->
    <<"immediately">>;
build_field(Key, Value) when Key == last_join; Key == first_reset; Key == last_reset;
                        Key == datetime; Key == devstat_time; Key == time;
                        Key == first_rx; Key == last_rx ->
    iso8601:format(Value);
build_field(Key, Value) when Key == devstat ->
    build_devstat(Value);
build_field(Key, Value) when Key == dwell ->
    lists:map(fun({Time, {Freq, Duration, Sum}}) ->
                #{time=>iso8601:format(Time), freq=>Freq, duration=>Duration, hoursum=>Sum}
              end, Value);
build_field(Key, Value) when Key == delays ->
    lists:map(fun({Date, SDelay, NDelay}) -> #{date=>iso8601:format(Date), srvdelay=>SDelay, nwkdelay=>NDelay};
                ({Date, NDelay}) -> #{date=>iso8601:format(Date), srvdelay=>undefined, nwkdelay=>NDelay}
              end, Value);
build_field(Key, Value) when Key == last_qs ->
    lists:map(fun(Item) -> build_qs(Item) end, Value);
build_field(Key, Value) when Key == average_qs ->
    build_qs(Value);
build_field(Key, Value) when Key == build; Key == parse ->
    build_fun(Value);
build_field(Key, Value) when Key == gateway ->
    build(Value);
build_field(_Key, Value) ->
    Value.

parse_bitstring(Map) ->
    case parse_opt(val, Map) of
        undefined ->
            case parse_opt(len, Map) of
                undefined ->
                    <<>>;
                Len ->
                    <<0:Len>>
            end;
        Val ->
            BinVal = lorawan_mac:hex_to_binary(Val),
            case parse_opt(len, Map) of
                undefined ->
                    BinVal;
                Len when Len =< bit_size(BinVal) ->
                    <<Res:Len/bitstring, _/bitstring>> = BinVal,
                    Res;
                Len when Len > bit_size(BinVal) ->
                    <<BinVal/binary, 0:(Len-bit_size(BinVal))>>
            end
    end.

build_bitstring(Value) ->
    case bit_size(Value) rem 8 of
        0 -> #{val => lorawan_mac:binary_to_hex(Value),
                len => bit_size(Value)};
        N -> #{val => lorawan_mac:binary_to_hex(<<Value/bitstring, 0:(8-N)>>),
                len => bit_size(Value)}
    end.

parse_latlon(List) ->
    {parse_opt(lat, List), parse_opt(lon, List)}.

build_latlon({Lat, Lon}) ->
    #{lat => build_opt(Lat), lon => build_opt(Lon)}.

parse_adr(List) ->
    {parse_opt(power, List), parse_opt(datr, List),
        case maps:get(chans, List, null) of
            null -> undefined;
            Val -> text_to_intervals(binary_to_list(Val))
        end}.

build_adr({TXPower, DataRate, Chans}) ->
    #{power => build_opt(TXPower), datr => build_opt(DataRate), chans =>
        case Chans of
            undefined -> null;
            Val -> list_to_binary(intervals_to_text(Val))
        end}.

parse_rxwin(List) ->
    {parse_opt(rx1_dr_offset, List),
        parse_opt(rx2_dr, List), parse_opt(rx2_freq, List)}.

build_rxwin({RX1DROffset, RX2DataRate, Frequency}) ->
    #{rx1_dr_offset => build_opt(RX1DROffset),
        rx2_dr => build_opt(RX2DataRate), rx2_freq => build_opt(Frequency)}.

parse_devstat(Value) ->
    lists:map(
        fun(#{datetime := DateTime, battery := Battery, margin := Margin, max_snr := MaxSNR}) ->
            {iso8601:parse(DateTime), Battery, Margin, MaxSNR}
        end, Value).

% backward compatibility
build_devstat({Battery, Margin}) ->
    [#{datetime => iso8601:format(calendar:universal_time()),
        battery => build_opt(Battery), margin => build_opt(Margin), max_snr => 0}];
build_devstat(Value) ->
    lists:map(
        fun({Timestamp, Battery, Margin, MaxSNR}) ->
            #{datetime => iso8601:format(Timestamp), battery => Battery, margin => Margin, max_snr => MaxSNR};
        % backward compatibility
        % REMOVE BEFORE RELEASING 0.4.11
        ({Timestamp, Battery, Margin}) ->
            #{datetime => iso8601:format(Timestamp), battery => Battery, margin => Margin, max_snr => 0}
        end, Value).

parse_qs(List) ->
    {parse_opt(rssi, List), parse_opt(snr, List)}.

build_qs({RSSI, SNR}) ->
    #{rssi => build_opt(RSSI), snr => build_opt(SNR)}.

parse_fun(<<>>) ->
    undefined;
parse_fun(Code) ->
    % try to parse the function
    {ok, Ts, _} = erl_scan:string(binary_to_list(Code)),
    {ok, Exprs} = erl_parse:parse_exprs(Ts),
    {value, Fun, _} = erl_eval:exprs(Exprs, []),
    {Code, Fun}.

build_fun({Code, _Fun}) ->
    Code.

build_opt(undefined) -> null;
build_opt(<<"undefined">>) -> null; %% temporary db consistency fix, to be removed after some time
build_opt(Value) -> Value.

parse_opt(Field, List, Default) ->
    case maps:get(Field, List, Default) of
        null -> Default;
        Value -> Value
    end.
parse_opt(Field, List) ->
    parse_opt(Field, List, undefined).

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

bitstring_test_() -> [
    ?_assertEqual(<<2:4>>, parse_bitstring(#{val => <<"20">>, len => 4})),
    ?_assertEqual(<<16#20,0>>, parse_bitstring(#{val => <<"20">>, len => 16})),
    ?_assertEqual(<<0>>, parse_bitstring(#{val => <<"00">>})),
    ?_assertEqual(<<0:7>>, parse_bitstring(#{len => 7})),
    ?_assertEqual(<<>>, parse_bitstring(#{})),
    ?_assertEqual(#{val => <<>>, len => 0}, build_bitstring(<<>>)),
    ?_assertEqual(#{val => <<"07">>, len => 8}, build_bitstring(<<7>>)),
    ?_assertEqual(#{val => <<"70">>, len => 4}, build_bitstring(<<7:4>>))
].

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
