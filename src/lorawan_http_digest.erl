%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_http_digest).
-export([authorization_header/2, header/2, nonce/1, nc/1, ha1/1, response/5, response/8]).

authorization_header(Scheme, Cred) ->
    {<<"authorization">>, header(Scheme, Cred)}.

header(basic, Cred) ->
    <<"Basic ", Cred/binary>>;
header(digest, Params) ->
    <<"Digest ", (encode_params(<<>>, Params))/binary>>.

nonce(Len) ->
    binary_to_hex(crypto:strong_rand_bytes(Len)).

nc(Nc) ->
    integer_to_hex(Nc, 8).

encode_params(<<>>, [First | Rest]) ->
    encode_params(encode_param(First), Rest);
encode_params(Acc, [First | Rest]) ->
    encode_params(<<Acc/binary, ", ", (encode_param(First))/binary>>, Rest);
encode_params(Acc, []) ->
    Acc.

encode_param({Name, Value})
        when Name == <<"algorithm">>; Name == <<"qop">>; Name == <<"nc">> ->
    <<Name/binary, $=, Value/binary>>;
encode_param({Name, Value}) ->
    <<Name/binary, $=, $", Value/binary, $">>.

ha1({User, Realm, Password}) ->
    binary_to_hex(crypto:hash(md5,
        <<User/binary, $:, Realm/binary, $:, Password/binary>>));
ha1(HA1) ->
    HA1.

response(Method, URI, _Body, Credentials, Nonce) ->
    HA1 = ha1(Credentials),
    HA2 = binary_to_hex(crypto:hash(md5,
        <<Method/binary, $:, URI/binary>>)),
    binary_to_hex(crypto:hash(md5,
        <<HA1/binary, $:, Nonce/binary, $:, HA2/binary>>)).

response(Method, URI, Body, Credentials, Nonce, Nc, CNonce, Qop) ->
    HA1 = ha1(Credentials),
    HA2 = binary_to_hex(crypto:hash(md5,
        case Qop of
            <<"auth-int">> ->
                BodyHash = binary_to_hex(crypto:hash(md5, Body)),
                <<Method/binary, $:, URI/binary, $:, BodyHash/binary>>;
            _Else ->
                <<Method/binary, $:, URI/binary>>
        end)),
    binary_to_hex(crypto:hash(md5,
        <<HA1/binary, $:, Nonce/binary, $:, Nc/binary, $:, CNonce/binary, $:, Qop/binary, $:, HA2/binary>>)).

-include_lib("eunit/include/eunit.hrl").

digest_test_() -> [
    ?_assertEqual(<<"6629fae49393a05397450978507c4ef1">>,
        response(<<"GET">>, <<"/dir/index.html">>, <<>>,
            {<<"Mufasa">>, <<"testrealm@host.com">>, <<"Circle Of Life">>},
            <<"dcd98b7102dd2f0e8b11d0f600bfb0c093">>,
            <<"00000001">>, <<"0a4f113b">>, <<"auth">>))].

integer_to_hex(Num, Len) ->
    list_to_binary(string:right(string:to_lower(integer_to_list(Num,16)), Len, $0)).

binary_to_hex(Id) ->
    << <<Y>> || <<X:4>> <= Id, Y <- string:to_lower(integer_to_list(X,16))>>.

% end of file
