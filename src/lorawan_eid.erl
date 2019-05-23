%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_eid).

% support for ID6 from https://doc.sm.tc/station/glossary.html#term-id6
-export([parse/1, as_id6/1]).

parse(Int) when is_integer(Int) ->
    <<Int:64>>;
parse(Binary) ->
    binarize(
        binary:split(Binary, [<<":">>, <<"-">>], [global]) ).

binarize([Single]) ->
    lorawan_utils:hex_to_binary(Single);
binarize(List) when length(List) == 8 ->
    list2bin(List);
binarize(List) ->
    split2bin(
        lists:map(
            fun (<<>>) -> <<>>;
                (Num) -> binary_to_integer(Num, 16)
            end,
            List)).

% standard dash- or semicolon-separated number
list2bin([X | Rest]) ->
    <<(binary_to_integer(X,16)), (list2bin(Rest))/binary>>;
list2bin([]) ->
    <<>>.

% ID6 strings
split2bin([<<>>,<<>>,<<>>]) ->
    <<00:64>>;
split2bin([<<>>,<<>>,D]) ->
    <<00:48,D:16>>;
split2bin([A,<<>>,<<>>]) ->
    <<A:16,00:48>>;
split2bin([<<>>,<<>>,C,D]) ->
    <<00:32,C:16,D:16>>;
split2bin([A,<<>>,D]) ->
    <<A:16,00:32,D:16>>;
split2bin([A,B,<<>>,<<>>]) ->
    <<A:16,B:16,00:32>>;
split2bin([<<>>,<<>>,B,C,D]) ->
    <<00:16,B:16,C:16,D:16>>;
split2bin([A,<<>>,C,D]) ->
    <<A:16,00:16,C:16,D:16>>;
split2bin([A,B,<<>>,D]) ->
    <<A:16,B:16,00:16,D:16>>;
split2bin([A,B,C,<<>>,<<>>]) ->
    <<A:16,B:16,C:16,00:16>>;
split2bin([A,B,C,D]) ->
    <<A:16,B:16,C:16,D:16>>.


as_id6(Bin) ->
    join(
        lists:map(
            fun (<<>>) -> <<>>;
                (Num) -> integer_to_binary(Num, 16)
            end,
            bin2split(Bin)),
        $:).

join(List, Sep) ->
    lists:foldr(
        fun (A, undefined) -> A;
            (A, B) -> <<A/binary, Sep, B/binary>>
        end, undefined, List).

bin2split(<<00:64>>) ->
    [<<>>,<<>>,<<>>];
bin2split(<<00:48,D:16>>) ->
    [<<>>,<<>>,D];
bin2split(<<A:16,00:48>>) ->
    [A,<<>>,<<>>];
bin2split(<<00:32,C:16,D:16>>) ->
    [<<>>,<<>>,C,D];
bin2split(<<A:16,00:32,D:16>>) ->
    [A,<<>>,D];
bin2split(<<A:16,B:16,00:32>>) ->
    [A,B,<<>>,<<>>];
bin2split(<<00:16,B:16,C:16,D:16>>) ->
    [<<>>,<<>>,B,C,D];
bin2split(<<A:16,00:16,C:16,D:16>>) ->
    [A,<<>>,C,D];
bin2split(<<A:16,B:16,00:16,D:16>>) ->
    [A,B,<<>>,D];
bin2split(<<A:16,B:16,C:16,00:16>>) ->
    [A,B,C,<<>>,<<>>];
bin2split(<<A:16,B:16,C:16,D:16>>) ->
    [A,B,C,D].

-include_lib("eunit/include/eunit.hrl").

id6_test_()-> [
    % examples from https://doc.sm.tc/station/glossary.html#term-id6
    ?_assertEqual(<<"::">>, as_id6(parse(<<"00-00-00-00-00-00-00-00">>))),
    ?_assertEqual(<<"1::">>, as_id6(parse(<<"00-01-00-00-00-00-00-00">>))),
    ?_assertEqual(<<"::A:B">>, as_id6(parse(<<"00-00-00-00-00-0a-00-0b">>))),
    ?_assertEqual(<<"F::1">>, as_id6(parse(<<"00-0f-00-00-00-00-00-01">>))),
    ?_assertEqual(<<"F:A123:F8:100">>, as_id6(parse(<<"00-0f-a1-23-00-f8-01-00">>)))
].

% end of file
