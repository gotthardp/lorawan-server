%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(loramote_tests).
-include_lib("eunit/include/eunit.hrl").

% fixture is my friend
loramote_test_() ->
    {setup,
        fun() ->
            {ok, _} = application:ensure_all_started(lorawan_server),
            lager:set_loglevel(lager_console_backend, debug),
            test_admin:add_device(<<"0000000000000000">>),
            test_admin:add_link(<<"11223344">>, <<"EU863-870">>, <<"2B7E151628AED2A6ABF7158809CF4F3C">>, <<"2B7E151628AED2A6ABF7158809CF4F3C">>)
        end,
        fun(_State) ->
            application:stop(lorawan_server)
        end,
        fun loramote_test/1}.

loramote_test(_State) ->
    [
    ?_assertEqual({error, timeout}, test_forwarder:push_and_pull(<<0,0,0,0,0,0,0,0>>, <<"bad_json">>)),
    % random messages from LoRa Mote
    ?_assertEqual({ok,{<<"YEQzIhEAAQAChkQA7Q4=">>}}, test_forwarder:push_and_pull(<<0,0,0,0,0,0,0,0>>, test_forwarder:rxpk("QEQzIhEABAACP24OaiNddeeybMAun0EwVHf4eaY="))),
    ?_assertEqual({ok,{<<"YEQzIhEBAgAGAnyjfkSq">>}}, test_forwarder:push_and_pull(<<0,0,0,0,0,0,0,0>>, test_forwarder:rxpk("QEQzIhEA0AACHaxbOsSlM9izylIPYNdD3QuCrXI=")))
    ].

% end of file
