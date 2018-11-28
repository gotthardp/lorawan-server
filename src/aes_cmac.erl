%% By Andreas Schultz (RoadRunnr)
%% https://gist.github.com/RoadRunnr/fc1cdba514030c0c7f6529ec6222989b
-module(aes_cmac).

%% R20 will have CMAC in crypto....
-export([aes_cmac/2, aes_cmac/3, generate_subkeys/1]).

-define(Zero, <<0:128>>).
-define(Rb, <<16#87:128>>).
-define(BlockSize, 16).

-spec aes_cmac(K :: <<_:128>>, M :: binary(), S :: integer()) -> binary().
aes_cmac(K, M, S) ->
    erlang:binary_part(aes_cmac(K, M), 0, S).

%%   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%   +                    Algorithm Generate_Subkey                      +
%%   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%   +                                                                   +
%%   +   Input    : K (128-bit key)                                      +
%%   +   Output   : K1 (128-bit first subkey)                            +
%%   +              K2 (128-bit second subkey)                           +
%%   +-------------------------------------------------------------------+
%%   +                                                                   +
%%   +   Constants: const_Zero is 0x00000000000000000000000000000000     +
%%   +              const_Rb   is 0x00000000000000000000000000000087     +
%%   +   Variables: L          for output of AES-128 applied to 0^128    +
%%   +                                                                   +
%%   +   Step 1.  L := AES-128(K, const_Zero);                           +
%%   +   Step 2.  if MSB(L) is equal to 0                                +
%%   +            then    K1 := L << 1;                                  +
%%   +            else    K1 := (L << 1) XOR const_Rb;                   +
%%   +   Step 3.  if MSB(K1) is equal to 0                               +
%%   +            then    K2 := K1 << 1;                                 +
%%   +            else    K2 := (K1 << 1) XOR const_Rb;                  +
%%   +   Step 4.  return K1, K2;                                         +
%%   +                                                                   +
%%   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec generate_subkeys(K :: <<_:128>>) -> {<<_:128>>, <<_:128>>}.
generate_subkeys(<<_:128>> = K) ->
    L = crypto:block_encrypt(aes_ecb, K, ?Zero),
    K1 = case L of
	     <<0:1, Rest0:127>> ->  <<(Rest0 bsl 1):128>>;
	     <<1:1, Rest0:127>> -> crypto:exor(<<(Rest0 bsl 1):128>>, ?Rb)
	 end,
    K2 = case K1 of
	     <<0:1, Rest1:127>> ->  <<(Rest1 bsl 1):128>>;
	     <<1:1, Rest1:127>> -> crypto:exor(<<(Rest1 bsl 1):128>>, ?Rb)
	 end,
    {K1, K2}.

%%   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%   +                   Algorithm AES-CMAC                              +
%%   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%   +                                                                   +
%%   +   Input    : K    ( 128-bit key )                                 +
%%   +            : M    ( message to be authenticated )                 +
%%   +            : len  ( length of the message in octets )             +
%%   +   Output   : T    ( message authentication code )                 +
%%   +                                                                   +
%%   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
%%   +   Constants: const_Zero is 0x00000000000000000000000000000000     +
%%   +              const_Bsize is 16                                    +
%%   +                                                                   +
%%   +   Variables: K1, K2 for 128-bit subkeys                           +
%%   +              M_i is the i-th block (i=1..ceil(len/const_Bsize))   +
%%   +              M_last is the last block xor-ed with K1 or K2        +
%%   +              n      for number of blocks to be processed          +
%%   +              r      for number of octets of last block            +
%%   +              flag   for denoting if last block is complete or not +
%%   +                                                                   +
%%   +   Step 1.  (K1,K2) := Generate_Subkey(K);                         +
%%   +   Step 2.  n := ceil(len/const_Bsize);                            +
%%   +   Step 3.  if n = 0                                               +
%%   +            then                                                   +
%%   +                 n := 1;                                           +
%%   +                 flag := false;                                    +
%%   +            else                                                   +
%%   +                 if len mod const_Bsize is 0                       +
%%   +                 then flag := true;                                +
%%   +                 else flag := false;                               +
%%   +                                                                   +
%%   +   Step 4.  if flag is true                                        +
%%   +            then M_last := M_n XOR K1;                             +
%%   +            else M_last := padding(M_n) XOR K2;                    +
%%   +   Step 5.  X := const_Zero;                                       +
%%   +   Step 6.  for i := 1 to n-1 do                                   +
%%   +                begin                                              +
%%   +                  Y := X XOR M_i;                                  +
%%   +                  X := AES-128(K,Y);                               +
%%   +                end                                                +
%%   +            Y := M_last XOR X;                                     +
%%   +            T := AES-128(K,Y);                                     +
%%   +   Step 7.  return T;                                              +
%%   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec aes_cmac(K :: <<_:128>>, M :: binary()) -> binary().
aes_cmac(<<_:128>> = K, M) ->
    case erlang:function_exported(crypto, cmac, 3) of
        true ->
            crypto:cmac(aes_cbc128, K, M);
        false ->
            {K1, K2} = generate_subkeys(K),
            calc(M, {K, K1, K2})
    end.

calc(M, {K, K1, K2}) ->
    calc(M, {K, K1, K2}, ?Zero).

calc(<<M0:?BlockSize/binary>>, {K, K1, _K2}, X) ->
    M1 = crypto:exor(M0, K1),
    Y = crypto:exor(M1, X),
    crypto:block_encrypt(aes_ecb, K, Y);

calc(<<M:?BlockSize/binary, Rest/binary>>, {K, K1, K2}, X0) ->
    Y = crypto:exor(X0, M),
    X1 = crypto:block_encrypt(aes_ecb, K, Y),
    calc(Rest, {K, K1, K2}, X1);

calc(M, {K, _K1, K2}, X) ->
    PaddingSize = 128 - bit_size(M) - 1,
    M1 = crypto:exor(<<M/binary, 1:1, 0:PaddingSize>>, K2),
    Y = crypto:exor(M1, X),
    crypto:block_encrypt(aes_ecb, K, Y).