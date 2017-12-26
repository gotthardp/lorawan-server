%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_connector).
-export([is_pattern/1, prepare_filling/1, fill_pattern/2, prepare_matching/1, match_vars/2]).
-export([form_encode/1, decode/2]).

is_pattern(Pattern) ->
    case string:chr(Pattern, ${) of
        0 -> false;
        N when N > 0 -> true
    end.

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

fill_pattern({Pattern, []}, _) ->
    Pattern;
fill_pattern({Pattern, Vars}, Values) ->
    maps:fold(
        fun(Var, Val, Patt) ->
            case proplists:get_value(Var, Vars, undefined) of
                {Start, Len} ->
                    <<Prefix:Start/binary, _:Len/binary, Suffix/binary>> = Patt,
                    <<Prefix/binary, Val/binary, Suffix/binary>>;
                undefined ->
                    Patt
            end
        end, Pattern, Values).

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

decode(<<"raw">>, Msg) ->
    {ok, #{data => Msg}};
decode(<<"json">>, Msg) ->
    case catch lorawan_admin:parse(jsx:decode(Msg, [return_maps, {labels, atom}])) of
        Struct when is_map(Struct) ->
            {ok, Struct};
        _Else ->
            {error, json_syntax_error}
    end.


-include_lib("eunit/include/eunit.hrl").

matchtst(undefined = Vars, Pattern, Topic) ->
    [?_assertEqual(Vars, match_pattern(Topic, prepare_matching(Pattern))),
    ?_assertEqual(Pattern, fill_pattern(prepare_filling(Pattern), Vars))];
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
    ?_assertEqual(#{devaddr => <<"00112233">>},
        match_pattern(<<"00112233/trailing/data">>, prepare_matching(<<"{devaddr}/#">>))),
    ?_assertEqual(#{devaddr => <<"00112233">>},
        match_pattern(<<"/leading/data/00112233">>, prepare_matching(<<"#/{devaddr}">>)))].


www_form_test_()-> [
    ?_assertEqual(<<>>, form_encode(#{})),
    ?_assertEqual(<<"one=1">>, form_encode(#{one=>1})),
    ?_assertEqual(<<"one=1&two=val">>, form_encode(#{one=>1,two=>"val"})),
    ?_assertEqual(<<"one=1&three=%26&two=val">>, form_encode(#{one=>1,two=>"val",three=><<"&">>}))
].

% end of file
