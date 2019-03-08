%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin).

-export([handle_authorization/2, handle_authorization_ex/2, fields_empty/1, auth_field/2]).
-export([write/1, parse/1, build/1]).
-export([parse_field/2, build_field/2]).
-export([timestamp_to_json_date/1]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

handle_authentication(Req) ->
    handle_authentication_field(Req,
        cowboy_req:parse_header(<<"authorization">>, Req)).

% when the client provided HTTP Basic password
handle_authentication_field(_Req, {basic, User, Pass}) ->
    case mnesia:dirty_read(user, User) of
        [#user{pass_ha1=HA1, scopes=AuthScopes}] ->
            case lorawan_http_digest:ha1({User, ?REALM, Pass}) of
                HA1 ->
                    {true, AuthScopes};
                _Else ->
                    {false, erlang:iolist_to_binary([<<"Basic realm=\"">>, ?REALM, $"])}
            end;
        [] ->
            {false, erlang:iolist_to_binary([<<"Basic realm=\"">>, ?REALM, $"])}
    end;
% when the client did respond to the HTTP Digest challenge
handle_authentication_field(Req, {digest, Params}) ->
    Method = cowboy_req:method(Req),
    UserName = proplists:get_value(<<"username">>, Params, <<>>),
    Nonce = proplists:get_value(<<"nonce">>, Params, <<>>),
    URI = proplists:get_value(<<"uri">>, Params, <<>>),
    Response = proplists:get_value(<<"response">>, Params, <<>>),
    % retrieve and check password
    case mnesia:dirty_read(user, UserName) of
        [#user{pass_ha1=HA1, scopes=AuthScopes}] ->
            case lorawan_http_digest:response(Method, URI, <<>>, HA1, Nonce) of
                Response ->
                    {true, AuthScopes};
                _Else ->
                    {false, digest_header()}
            end;
        [] ->
            {false, digest_header()}
    end;
% if nothing was provided
handle_authentication_field(_Req, _Else) ->
    {false, digest_header()}.

handle_authorization(Req, {Read, Write}) ->
    case handle_authentication(Req) of
        {true, AuthScopes} ->
            case lists:member(cowboy_req:method(Req), [<<"OPTIONS">>, <<"GET">>]) of
                true ->
                    {true, authorized_fields(AuthScopes, Read++Write)};
                false ->
                    {true, authorized_fields(AuthScopes, Write)}
            end;
        {false, Header} ->
            {false, Header}
    end;
handle_authorization(Req, Read) ->
    handle_authorization(Req, {Read, []}).

handle_authorization_ex(Req, {Read, Write}) ->
    case handle_authentication(Req) of
        {true, AuthScopes} ->
            {true, authorized_fields(AuthScopes, Read++Write), authorized_fields(AuthScopes, Write)};
        {false, Header} ->
            {false, Header}
    end;
handle_authorization_ex(Req, Read) ->
    handle_authorization_ex(Req, {Read, []}).

fields_empty('*') ->
    false;
fields_empty(List) when length(List) > 0 ->
    false;
fields_empty([]) ->
    true.

auth_field(_, '*') ->
    true;
auth_field(Field, AuthFields) ->
    lists:member(Field, AuthFields).

digest_header() ->
    Nonce = lorawan_http_digest:nonce(16),
    lorawan_http_digest:header(digest, [
        {<<"realm">>, ?REALM}, {<<"nonce">>, Nonce}, {<<"domain">>, <<"/">>}]).

authorized_fields(AuthScopes, ReqScopes) ->
    merge_scopes(
        authorized_scopes(AuthScopes, ReqScopes)).

authorized_scopes(undefined, ReqScopes) ->
    % temporary provisions for backward compatibility
    ReqScopes;
authorized_scopes(AuthScopes, ReqScopes) ->
    case lists:member(<<"unlimited">>, AuthScopes) of
        true ->
            ReqScopes;
        false ->
            lists:filter(
                fun
                    ({'*', _}) -> true;
                    ({Name, _}) -> lists:member(Name, AuthScopes)
                end,
                ReqScopes)
    end.

merge_scopes(Scopes) ->
    lists:foldl(
        fun
            (_, '*') -> '*';
            ({_, '*'}, _) -> '*';
            ({_, Fields}, Acc) -> Fields ++ Acc
        end,
        [], Scopes).

write(Rec) ->
    mnesia:write(lorawan_db_guard:update_health(Rec)).

parse(Object) when is_map(Object) ->
    maps:map(
        fun
            (eid, Value) -> parse_eid(Value, Object);
            (Key, Value) -> parse_field(Key, Value)
        end,
        Object).

build(Object) when is_map(Object) ->
    maps:map(
        fun
            (_Key, Value) when is_map(Value) -> build(Value);
            (eid, Value) -> build_eid(Value, Object);
            (Key, Value) -> build_field(Key, Value)
        end,
        maps:filter(
            fun
                (_Key, undefined) -> false;
                (_, _) -> true
            end, Object));
build(Object) when is_list(Object) ->
    lists:map(
        fun (Value) -> build(Value) end,
        Object).

parse_eid(Value, _) when Value == null; Value == undefined ->
    undefined;
parse_eid(Value, #{entity:=Entity}) when Entity == server; Entity == connector ->
    Value;
parse_eid(Value, _) ->
    lorawan_utils:hex_to_binary(Value).

parse_field(_Key, Value) when Value == null; Value == undefined ->
    undefined;
parse_field(Key, Value) when Key == mac; Key == netid; Key == mask;
                        Key == deveui; Key == appeui; Key == appkey; Key == node;
                        Key == devaddr; Key == nwkskey; Key == appskey;
                        Key == data; Key == frid; Key == evid; Key == eid ->
    lorawan_utils:hex_to_binary(Value);
parse_field(Key, Value) when Key == sname; Key == severity; Key == entity ->
    binary_to_existing_atom(Value, latin1);
parse_field(Key, Value) when Key == gateways ->
    lists:map(
        fun(#{mac:=MAC, rxq:=RxQ}) ->
            {lorawan_utils:hex_to_binary(MAC), ?to_record(rxq, parse(RxQ))}
        end, Value);
parse_field(Key, Value) when Key == subid ->
    parse_bitstring(Value);
parse_field(Key, Values) when Key == cflist ->
    lists:map(
        fun(#{freq:=Freq}=Map) ->
            {Freq, parse_opt(min_datr, Map), parse_opt(max_datr, Map)}
        end, Values);
parse_field(Key, Value) when Key == gpspos ->
    parse_latlon(Value);
parse_field(Key, Value) when Key == adr_use; Key == adr_set ->
    parse_adr(Value);
parse_field(Key, Value) when Key == init_chans ->
    text_to_intervals(binary_to_list(Value));
parse_field(Key, Value) when Key == rxwin_init; Key == rxwin_set; Key == rxwin_use ->
    parse_rxwin(Value);
parse_field(Key, Value) when Key == rxq ->
    ?to_record(rxq, parse(Value));
parse_field(Key, Value) when Key == txdata ->
    ?to_record(txdata, parse(Value));
parse_field(Key, Value) when Key == last_joins ->
    lists:map(
        fun(#{time:=Time, dev_nonce:=DevNonce}) ->
            {iso8601:parse(Time), lorawan_utils:hex_to_binary(DevNonce)}
        end, Value);
parse_field(Key, Value) when Key == first_reset; Key == last_reset;
                        Key == last_alive; Key == last_report;
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
        fun(#{date:=Date, min:=Min, avg:=Avg, max:=Max}) ->
            {iso8601:parse(Date), {Min, Avg, Max}}
        end, Value);
parse_field(Key, Value) when Key == router_perf ->
    lists:map(
        fun(#{date:=Date, request_cnt:=ReqCnt, error_cnt:=ErrCnt}) ->
            {iso8601:parse(Date), {ReqCnt, ErrCnt}}
        end, Value);
parse_field(Key, Value) when Key == last_qs ->
    lists:map(fun(Item) -> parse_qs(Item) end, Value);
parse_field(Key, Value) when Key == average_qs ->
    parse_qs(Value);
parse_field(Key, Value) when Key == parse_uplink; Key == parse_event; Key == build ->
    parse_fun(Value);
parse_field(Key, #{ip:=IP, port:=Port, ver:=Ver}) when Key == ip_address ->
    {ok, IP2} = inet_parse:address(binary_to_list(IP)),
    {IP2, Port, Ver};
parse_field(_Key, Value) ->
    Value.

build_eid(undefined, _) ->
    null;
build_eid(Value, #{entity:=Entity}) when Entity == server; Entity == connector ->
    Value;
build_eid(Value, _) ->
    lorawan_utils:binary_to_hex(Value).

build_field(_Key, undefined) ->
    null;
build_field(Key, Value) when Key == mac; Key == netid; Key == mask;
                        Key == deveui; Key == appeui; Key == appkey; Key == node;
                        Key == devaddr; Key == nwkskey; Key == appskey;
                        Key == data; Key == frid; Key == evid; Key == eid ->
    lorawan_utils:binary_to_hex(Value);
build_field(Key, Value) when Key == sname; Key == severity; Key == entity ->
    atom_to_binary(Value, latin1);
build_field(Key, Value) when Key == gateways ->
    lists:map(
        fun({MAC, RxQ}) ->
            #{mac=>lorawan_utils:binary_to_hex(MAC), rxq=>build(?to_map(rxq, RxQ))}
        end, Value);
build_field(Key, Value) when Key == subid ->
    build_bitstring(Value);
build_field(Key, Values) when Key == cflist ->
    lists:map(
        fun
            ({Freq, MinDR, MaxDR}) ->
                #{freq=>Freq, min_datr=>build_opt(MinDR), max_datr=>build_opt(MaxDR)};
            % backwards compatibility
            (Freq) ->
                #{freq=>Freq}
        end, Values);
build_field(Key, Value) when Key == gpspos ->
    build_latlon(Value);
build_field(Key, Value) when Key == adr_use; Key == adr_set ->
    build_adr(Value);
build_field(Key, Value) when Key == init_chans ->
    list_to_binary(intervals_to_text(Value));
build_field(Key, Value) when Key == rxwin_init; Key == rxwin_set; Key == rxwin_use ->
    build_rxwin(Value);
build_field(Key, Value) when Key == rxq ->
    build(?to_map(rxq, Value));
build_field(Key, Value) when Key == txdata ->
    build(?to_map(txdata, Value));
build_field(Key, immediately) when Key == time ->
    <<"immediately">>;
build_field(Key, Value) when Key == last_joins ->
    lists:map(fun({Time, DevNonce}) ->
                #{time=>iso8601:format(Time), dev_nonce=>lorawan_utils:binary_to_hex(DevNonce)}
              end, Value);
build_field(Key, Value) when Key == first_reset; Key == last_reset;
                        Key == datetime; Key == devstat_time; Key == time;
                        Key == last_alive; Key == last_report;
                        Key == first_rx; Key == last_rx ->
    iso8601:format(Value);
build_field(Key, Value) when Key == devstat ->
    build_devstat(Value);
build_field(Key, Value) when Key == dwell ->
    lists:map(fun({Time, {Freq, Duration, Sum}}) ->
                #{time=>iso8601:format(Time), freq=>Freq, duration=>Duration, hoursum=>Sum}
              end, Value);
build_field(Key, Value) when Key == delays ->
    lists:map(fun({Date, {Min, Avg, Max}}) ->
                  #{date=>iso8601:format(Date), min=>Min, avg=>Avg, max=>Max};
                 % backward compatibility with 0.4.x
                 ({Date, _SDelay, NDelay}) ->
                  #{date=>iso8601:format(Date), min=>NDelay, avg=>NDelay, max=>NDelay}
              end, Value);
build_field(Key, Value) when Key == router_perf ->
    lists:map(fun({Date, {ReqCnt, ErrCnt}}) ->
                  #{date=>iso8601:format(Date), request_cnt=>ReqCnt, error_cnt=>ErrCnt}
              end, Value);
build_field(Key, Value) when Key == last_qs ->
    lists:map(fun(Item) -> build_qs(Item) end, Value);
build_field(Key, Value) when Key == average_qs ->
    build_qs(Value);
build_field(Key, Value) when Key == parse_uplink; Key == parse_event; Key == build ->
    build_fun(Value);
build_field(Key, Value) when Key == all_gw ->
    lists:map(
        fun(Gw) -> build(Gw) end,
        Value);
build_field(Key, {IP, Port, Ver}) when Key == ip_address ->
    #{ip=>list_to_binary(inet_parse:ntoa(IP)), port=>Port, ver=>Ver};
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
            BinVal = lorawan_utils:hex_to_binary(Val),
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
        0 -> #{val => lorawan_utils:binary_to_hex(Value),
                len => bit_size(Value)};
        N -> #{val => lorawan_utils:binary_to_hex(<<Value/bitstring, 0:(8-N)>>),
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
        end};
build_adr(_Else) ->
    #{}.

parse_rxwin(List) ->
    {parse_opt(rx1_dr_offset, List),
        parse_opt(rx2_dr, List), parse_opt(rx2_freq, List)}.

build_rxwin({RX1DROffset, RX2DataRate, Frequency}) ->
    #{rx1_dr_offset => build_opt(RX1DROffset),
        rx2_dr => build_opt(RX2DataRate), rx2_freq => build_opt(Frequency)};
build_rxwin(_Else) ->
    #{}.

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

timestamp_to_json_date({{Yr,Mh,Dy},{Hr,Me,Sc}}) ->
    list_to_binary(
        lists:concat(["Date(",
            % javascript counts months 0-11
            integer_to_list(Yr), ",", integer_to_list(Mh-1), ",", integer_to_list(Dy), ",",
            integer_to_list(Hr), ",", integer_to_list(Me), ",", integer_to_list(trunc(Sc)), ")"]));
timestamp_to_json_date(_Else) ->
    null.

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
