%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector).
-export([node_to_vars/1, pid_to_binary/1, pid_to_binary/2, is_pattern/1, pattern_for_cowboy/1]).
-export([prepare_filling/1, fill_pattern/2, prepare_matching/1, match_vars/2, same_common_vars/2]).
-export([shared_access_token/4]).
-export([form_encode/1, decode_and_downlink/3]).
-export([raise_failed/2]).

-include("lorawan_db.hrl").

node_to_vars(#node{devaddr=DevAddr, appargs=AppArgs}) ->
    #{devaddr=>DevAddr, appargs=>AppArgs};
node_to_vars({#device{appargs=AppArgs}, DevAddr}) ->
    #{devaddr=>DevAddr, appargs=>AppArgs}.

pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

pid_to_binary(Pid, Idx) ->
    <<(pid_to_binary(Pid))/binary, "/", (integer_to_binary(Idx))/binary>>.

is_pattern(Pattern) ->
    case string:chr(Pattern, ${) of
        0 -> false;
        N when N > 0 -> true
    end.

pattern_for_cowboy(Empty)
        when Empty == undefined; Empty == <<>> ->
    undefined;
pattern_for_cowboy(<<"/", _/binary>>=URI) ->
    % convert our pattern to cowboy pattern
    re:replace(URI, "{([^}]+)}", ":\\1", [{return, binary}]);
pattern_for_cowboy(_Error) ->
    error.

-type fill_pattern_t() :: 'undefined' | {binary(), [{integer(), integer()}]}.
-spec prepare_filling('undefined' | binary() | [binary()]) -> fill_pattern_t() | [fill_pattern_t()].
prepare_filling(List) when is_list(List) ->
    lists:map(
        fun(Item) -> prepare_filling(Item) end, List);
prepare_filling(undefined) ->
    undefined;
prepare_filling(Pattern) ->
    case re:run(Pattern, "{[^}]+}", [global]) of
        {match, Match} ->
            {Pattern,
                [{binary_to_existing_atom(binary:part(Pattern, Start+1, Len-2), latin1), {Start, Len}}
                    || [{Start, Len}] <- Match]};
        nomatch ->
            {Pattern, []}
    end.

-spec fill_pattern(fill_pattern_t() | [fill_pattern_t()], map()) -> 'undefined' | binary() | [binary()].
fill_pattern(List, Values) when is_list(List) ->
    lists:map(
        fun(Item) -> fill_pattern(Item, Values) end, List);
fill_pattern(undefined, _) ->
    undefined;
fill_pattern({Pattern, []}, _) ->
    Pattern;
fill_pattern({Pattern, Vars}, Values) ->
    lists:foldr(
        fun({Var, {Start, Len}}, Patt) ->
            case get_value(Var, Values) of
                undefined ->
                    Patt;
                Val ->
                    <<Prefix:Start/binary, _:Len/binary, Suffix/binary>> = Patt,
                    <<Prefix/binary, Val/binary, Suffix/binary>>
            end
        end, Pattern, Vars).

get_value(Var, Values) when is_map(Values) ->
    case maps:is_key(Var, Values) of
        true ->
            maps:get(Var, Values);
        false ->
            % try searching recursively
            get_value(Var, maps:values(Values))
    end;
get_value(Var, [First | Values]) ->
    case get_value(Var, First) of
        undefined -> get_value(Var, Values);
        Val -> Val
    end;
get_value(_Var, _Else) ->
    undefined.

prepare_matching(undefined) ->
    undefined;
prepare_matching(Pattern) ->
    EPattern0 = binary:replace(Pattern, <<".">>, <<"\\">>, [global, {insert_replaced, 1}]),
    EPattern = binary:replace(EPattern0, <<"#">>, <<".*">>, [global]),
    case re:run(EPattern, "{[^}]+}", [global]) of
        {match, Match} ->
            Regex = lists:foldr(
                fun([{Start, Len}], Patt) ->
                    <<Prefix:Start/binary, _:Len/binary, Suffix/binary>> = Patt,
                    <<Prefix/binary, "([a-zA-z0-9]*)", Suffix/binary>>
                end, EPattern, Match),
            {ok, MP} = re:compile(<<"^", Regex/binary, "$">>),
            {MP, [binary_to_existing_atom(binary:part(EPattern, Start+1, Len-2), latin1) || [{Start, Len}] <- Match]};
        nomatch ->
            {Pattern, []}
    end.

match_pattern(Topic, {Pattern, Vars}) ->
    case re:run(Topic, Pattern, [global, {capture, all, binary}]) of
        {match, [[_Head | Matches]]} ->
            maps:from_list(lists:zip(Vars, Matches));
        nomatch ->
            undefined
    end.

match_vars(Topic, Pattern) ->
    case match_pattern(Topic, Pattern) of
        undefined ->
            lager:error("Topic ~w does not match pattern ~w", [Topic, Pattern]),
            #{};
        Vars ->
            lorawan_admin:parse(Vars)
    end.

same_common_vars(Vars1, Vars2) ->
    same_common_vars0(maps:to_list(Vars1), Vars2).

same_common_vars0([], _Vars2) ->
    true;
same_common_vars0([{_Key, undefined} | Vars1], Vars2) ->
    same_common_vars0(Vars1, Vars2);
same_common_vars0([{Key, Val} | Vars1], Vars2) ->
    case maps:get(Key, Vars2, undefined) of
        undefined ->
            same_common_vars0(Vars1, Vars2);
        Val ->
            same_common_vars0(Vars1, Vars2);
        _OtherVal ->
            false
    end.

% Shared Access Signature functions
% see https://docs.microsoft.com/en-us/azure/storage/storage-dotnet-shared-access-signature-part-1

shared_access_token(HostName, DeviceID, undefined, AccessKey) ->
    Res = lists:flatten(
        io_lib:format("~s/devices/~s", [HostName, DeviceID])),
    lists:flatten(
        build_access_token(Res, AccessKey));

shared_access_token(HostName, _DeviceID, KeyName, AccessKey) ->
    Res = lists:flatten(
        io_lib:format("~s/devices", [HostName])),
    lists:flatten(
        [build_access_token(Res, AccessKey), io_lib:format("&skn=~s", [KeyName])]).

build_access_token(Res0, AccessKey) ->
    build_access_token(Res0, AccessKey, 60*60*24*7). % expires in a week

build_access_token(Res0, AccessKey, Expiry) ->
    Res = http_uri:encode(Res0),
    % seconds since the UNIX epoch
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time())
     - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    ToSign = lists:flatten(
        io_lib:format("~s~n~B", [Res, Now+Expiry])),
    Sig = http_uri:encode(base64:encode_to_string(
        crypto:hmac(sha256, base64:decode(AccessKey), ToSign))),
    io_lib:format("SharedAccessSignature sr=~s&sig=~s&se=~B", [Res, Sig, Now+Expiry]).


% content formatting

form_encode(Values) ->
    cow_qs:qs(
        lists:map(
            fun({Name, Value}) ->
                {atom_to_binary(Name, latin1), value_to_binary(Value)}
            end,
            maps:to_list(lorawan_admin:build(Values)))).

value_to_binary(Term) when is_list(Term) -> list_to_binary(Term);
value_to_binary(Term) when is_binary(Term) -> Term;
value_to_binary(Term) -> list_to_binary(io_lib:print(Term)).

decode_and_downlink(#connector{app=App, format=Format}, Msg, Bindings) ->
    case decode(Format, Msg) of
        {ok, Vars} ->
            lorawan_application_backend:handle_downlink(App,
                maps:merge(Bindings, Vars));
        Error ->
            Error
    end.

decode(<<"raw">>, Msg) ->
    {ok, #{data => Msg}};
decode(<<"json">>, Msg) ->
    case catch lorawan_admin:parse(jsx:decode(Msg, [return_maps, {labels, atom}])) of
        Struct when is_map(Struct) ->
            {ok, Struct};
        _Else ->
            {error, json_syntax_error}
    end.

raise_failed(ConnId, {Error, Args}) ->
    lorawan_utils:throw_error({connector, ConnId}, {Error, Args}),
    {atomic, ok} = append_failed(ConnId, Error);
raise_failed(ConnId, Error) ->
    lorawan_utils:throw_error({connector, ConnId}, Error),
    {atomic, ok} = append_failed(ConnId, Error).

append_failed(ConnId, Error) ->
    mnesia:transaction(
        fun() ->
            [Rec] = mnesia:read(connector, ConnId, write),
            lorawan_admin:write(append_failed0(Rec, atom_to_binary(Error, latin1)))
        end).

append_failed0(#connector{failed=Failed}=Conn, Error) when is_list(Failed) ->
    Conn#connector{failed=[Error|Failed]};
append_failed0(Conn, Error) ->
    Conn#connector{failed=[Error]}.


-include_lib("eunit/include/eunit.hrl").

matchtst(undefined = Vars, Pattern, Topic) ->
    [?_assertEqual(Vars, match_pattern(Topic, prepare_matching(Pattern))),
    ?_assertEqual(Pattern, fill_pattern(prepare_filling(Pattern), #{}))];
matchtst(Vars, Pattern, Topic) ->
    [?_assertEqual(Vars, match_pattern(Topic, prepare_matching(Pattern))),
    ?_assertEqual(Topic, fill_pattern(prepare_filling(Pattern), Vars))].

pattern_test_()-> [
    matchtst(#{}, <<"normal/uri">>, <<"normal/uri">>),
    matchtst(undefined, <<"normal/uri">>, <<"another/uri">>),
    matchtst(#{devaddr => <<"00112233">>}, <<"{devaddr}">>, <<"00112233">>),
    matchtst(#{devaddr => <<"00112233">>}, <<"prefix.{devaddr}">>, <<"prefix.00112233">>),
    matchtst(#{devaddr => <<"00112233">>}, <<"{devaddr}/suffix">>, <<"00112233/suffix">>),
    matchtst(#{devaddr => <<"00112233">>}, <<"prefix:{devaddr}:suffix">>, <<"prefix:00112233:suffix">>),
    matchtst(#{group => <<"test">>, devaddr => <<"00112233">>}, <<"{group}-{devaddr}">>, <<"test-00112233">>),
    matchtst(#{a => <<"aaa">>, b => <<"b">>, c => <<"ccc">>, d => <<"d">>}, <<"{a}-{b}.{c}/{d}">>, <<"aaa-b.ccc/d">>),
    ?_assertEqual(<<"{unknown}/00112233">>,
        fill_pattern(prepare_filling(<<"{unknown}/{devaddr}">>), #{devaddr => <<"00112233">>})),
    ?_assertEqual(#{devaddr => <<"00112233">>},
        match_pattern(<<"00112233/trailing/data">>, prepare_matching(<<"{devaddr}/#">>))),
    ?_assertEqual(#{devaddr => <<"00112233">>},
        match_pattern(<<"/leading/data/00112233">>, prepare_matching(<<"#/{devaddr}">>))),
    ?_assertEqual(<<"/without/template">>, pattern_for_cowboy(<<"/without/template">>)),
    ?_assertEqual(<<"/some/:template">>, pattern_for_cowboy(<<"/some/{template}">>))].


www_form_test_()-> [
    ?_assertEqual(<<>>, form_encode(#{})),
    ?_assertEqual(<<"one=1">>, form_encode(#{one=>1})),
    ?_assertEqual(<<"one=1&two=val">>, form_encode(#{one=>1,two=>"val"})),
    ?_assertEqual(<<"one=1&three=%26&two=val">>, form_encode(#{one=>1,two=>"val",three=><<"&">>}))
].

% end of file
