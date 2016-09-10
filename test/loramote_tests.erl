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
            test_admin:add_device(<<"0000000000000000">>),
            test_admin:add_link(<<"72712B4A">>, <<"2B7E151628AED2A6ABF7158809CF4F3C">>, <<"2B7E151628AED2A6ABF7158809CF4F3C">>)
        end,
        fun(_State) ->
            application:stop(lorawan_server)
        end,
        fun loramote_test/1}.

loramote_test(_State) ->
    [
    ?_assertEqual({error, timeout}, test_forwarder:push_and_pull(<<0,0,0,0,0,0,0,0>>, <<"bad_json">>))
%    ?_assertEqual(ok, test_forwarder:push_and_pull(<<0,0,0,0,0,0,0,0>>, test_forwarder:rxpk(<<"IJ+qrB+s17zlqkQNfsokit0">>)))
    ].

% end of file
