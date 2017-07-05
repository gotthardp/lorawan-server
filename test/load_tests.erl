%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(load_tests).
-include_lib("eunit/include/eunit.hrl").

-define(GW_COUNT, 5).
-define(NODES_PER_GW, 5).
-define(FRAMES_PER_NODE, 5).

-define(NWKSKEY, <<"2B7E151628AED2A6ABF7158809CF4F3C">>).
-define(APPSKEY, <<"2B7E151628AED2A6ABF7158809CF4F3C">>).

-record(state, {gateways, nodes}).

% fixture is my friend
load_test_() ->
    {setup,
        fun() ->
            {ok, _} = application:ensure_all_started(lorawan_server),
            lager:set_loglevel(lager_console_backend, debug),
            Gateways =
                lists:map(
                    fun(ID) ->
                        MAC = <<0,0,0,0,0,0,0,ID>>,
                        test_admin:add_gateway(MAC),
                        {ok, Gateway} = test_forwarder:start_link(MAC, {"localhost", 1680}),
                        {ID, Gateway}
                    end,
                    lists:seq(1,?GW_COUNT)),
            Nodes =
                lists:foldl(
                    fun({ID1, Gateway}, Acc) ->
                        lists:foldl(
                            fun(ID2, Acc2) ->
                                NodeCfg = {<<0,0,ID1,ID2>>, ?NWKSKEY, ?APPSKEY},
                                test_admin:add_node(NodeCfg),
                                {ok, Node} = test_mote:start_link(NodeCfg, Gateway),
                                [Node | Acc2]
                            end,
                            Acc, lists:seq(1,?NODES_PER_GW))
                    end,
                    [], Gateways),
            #state{gateways=Gateways, nodes=Nodes}
        end,
        fun(#state{gateways=Gateways, nodes=Nodes}) ->
            lists:foreach(
                fun({_ID, Gateway}) ->
                    test_forwarder:stop(Gateway)
                end,
                Gateways),
            lists:foreach(
                fun(Node) ->
                    test_mote:stop(Node)
                end,
                Nodes),
            application:stop(lorawan_server),
            application:stop(mnesia)
        end,
        fun load_test/1}.

load_test(#state{nodes=Nodes}) ->
    {inparallel, 100,
        lists:map(
            fun(Node) ->
                send_receive(Node, ?FRAMES_PER_NODE)
            end,
            Nodes)}.

send_receive(Node, N) ->
    {inorder,
        lists:map(
            fun(Seq) ->
                ?_assertEqual({ok, 2, <<((Seq+1) rem 2)>>}, test_mote:push_and_pull(Node, Seq, 2, test_mote:semtech_payload(Seq)))
            end,
            lists:seq(1, N))}.

% end of file
