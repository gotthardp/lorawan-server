%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_db_guard).
-behaviour(gen_server).

-export([purge_queued/1, update_health/1, check_health/5, send_alert/7]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([check_alive/1, check_dwell/1]).
-export([check_reset/1, check_battery/1, check_margin/1, check_adr/1, check_rxwin/1]).
-export([check_failed/1]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    ok = mnesia:wait_for_tables([node], 2000),
    {ok, _} = mnesia:subscribe({table, node, simple}),
    {ok, _} = timer:send_interval(1000, monitor),
    {ok, _} = timer:send_interval(3600*1000, trim_tables),
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mnesia_table_event, {delete, {node, DevAddr}, _Id}}, State) ->
    node_deleted(DevAddr),
    {noreply, State};
handle_info(monitor, State) ->
    lorawan_db:foreach_record(gateway,
        mnesia:dirty_select(gateway,
            [{#gateway{mac='$1', health_next='$2', _='_'},
            [{'andalso', {'is_tuple', '$2'}, {'<', '$2', {const, calendar:universal_time()}}}], ['$1']}]),
        fun update_health/1),
    lorawan_db:foreach_record(node,
        mnesia:dirty_select(node,
            [{#node{devaddr='$1', health_next='$2', _='_'},
            [{'andalso', {'is_tuple', '$2'}, {'<', '$2', {const, calendar:universal_time()}}}], ['$1']}]),
        fun update_health/1),
    {noreply, State};
handle_info(trim_tables, State) ->
    trim_rxframes(),
    [mnesia:dirty_delete(event, E) || E <- expired_events()],
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


update_health(#gateway{mac=MAC, area=Area, dwell=Dwell,
        health_alerts=Alerts0, health_reported=Reported0} = Gateway0) ->
    % expire old data
    Gateway = Gateway0#gateway{dwell=lorawan_gw_router:update_dwell([], Dwell)},
    % perform the medical check
    {Reports, Alerts, Decay, Reported, MinNext} =
        check_health(Gateway, Alerts0, Reported0,
            ?MODULE, [check_alive, check_dwell]),
    case Reports of
        {NewAlerts, OtherAlerts} ->
            case mnesia:dirty_read(area, Area) of
                [#area{admins=Admins, slack_channel=Channel}] ->
                    send_alert(Admins, Channel, "gateway", lorawan_utils:binary_to_hex(MAC), NewAlerts, OtherAlerts, Decay);
                _Else ->
                    ok
            end;
        undefined ->
            ok
    end,
    Gateway#gateway{health_alerts=Alerts, health_decay=Decay, health_reported=Reported, health_next=MinNext};
update_health(#node{devaddr=DevAddr,
        health_alerts=Alerts0, health_reported=Reported0} = Node) ->
    {Reports, Alerts, Decay, Reported, MinNext} =
        check_health(Node, Alerts0, Reported0,
            ?MODULE, [check_reset, check_battery, check_margin, check_adr, check_rxwin]),
    case Reports of
        {NewAlerts, OtherAlerts} ->
            case lorawan_db:get_group(Node) of
                #group{admins=Admins, slack_channel=Channel} ->
                    send_alert(Admins, Channel, "node", lorawan_utils:binary_to_hex(DevAddr), NewAlerts, OtherAlerts, Decay);
                _Else ->
                    ok
            end;
        undefined ->
            ok
    end,
    Node#node{health_alerts=Alerts, health_decay=Decay, health_reported=Reported, health_next=MinNext};
update_health(#connector{connid=ConnId, app=AppId,
        health_alerts=Alerts0, health_reported=Reported0} = Connector) ->
    {Reports, Alerts, Decay, Reported, MinNext} =
        check_health(Connector, Alerts0, Reported0,
            ?MODULE, [check_failed]),
    case Reports of
        {NewAlerts, OtherAlerts} ->
            AffectedGroups =
                lists:usort(
                    [Gr || #profile{group=Gr} <- mnesia:dirty_index_read(profile, AppId, #profile.app)]),
            lists:foreach(
                fun(Group) ->
                    case mnesia:dirty_read(group, Group) of
                        #group{admins=Admins, slack_channel=Channel} ->
                            send_alert(Admins, Channel, "connector", ConnId, NewAlerts, OtherAlerts, Decay);
                        _Else ->
                            ok
                    end
                end, AffectedGroups);
        undefined ->
            ok
    end,
    Connector#connector{health_alerts=Alerts, health_decay=Decay, health_reported=Reported, health_next=MinNext};
update_health(Else) ->
    Else.

check_health(Rec, Alerts0, Reported0, Module, Funs) ->
    case check_health0(Rec, Module, Funs) of
        {[], MinNext} ->
            % clear all the flags
            {undefined, [], 0, 0, MinNext};
        {List, MinNext} ->
            {Alerts, Decays} = lists:unzip(List),
            Decay = lists:sum(Decays),
            NewAlerts =
                if
                    Alerts0 == undefined -> Alerts;
                    true -> lists:subtract(Alerts, Alerts0)
                end,
            if
                length(NewAlerts) > 0 ->
                    % new alerts raised
                    OtherAlerts = lists:subtract(Alerts, NewAlerts),
                    {{sublist(List, NewAlerts), sublist(List, OtherAlerts)},
                        Alerts, Decay, Decay, MinNext};
                Reported0 - Decay > 15 ->
                    % decay decreased a lot
                    {undefined, Alerts, Decay, Decay, MinNext};
                Reported0 < 70, Decay > 85 ->
                    % no new alerts, but decay increased a lot since last report
                    {{[], List}, Alerts, Decay, Decay, MinNext};
                true ->
                    {undefined, Alerts, Decay, Reported0, MinNext}
            end;
        undefined ->
            {undefined, undefined, undefined, undefined, undefined}
    end.

check_health0(Rec, Module, Funs) ->
    lists:foldl(
        fun
            (_Fun, undefined) ->
                undefined;
            (Fun, {Alerts, MinNext}) ->
                case apply(Module, Fun, [Rec]) of
                    ok ->
                        {Alerts, MinNext};
                    {ok, Next} when MinNext == undefined; Next < MinNext ->
                        {Alerts, Next};
                    {ok, _Next} ->
                        {Alerts, MinNext};
                    {Alert, Decay} ->
                        {[{Alert, Decay} | Alerts], MinNext};
                    MoreAlerts when is_list(MoreAlerts) ->
                        {MoreAlerts++Alerts, MinNext};
                    {Alert, Decay, Next} when MinNext == undefined; Next < MinNext ->
                        {[{Alert, Decay} | Alerts], Next};
                    {Alert, Decay, _Next} ->
                        {[{Alert, Decay} | Alerts], MinNext};
                    undefined ->
                        undefined
                end
        end,
        {[], undefined}, Funs).

sublist(Proplist, Keys) ->
    lists:foldl(
        fun(Key, List) ->
            [proplists:lookup(Key, Proplist) | List]
        end,
        [], Keys).

send_alert(Admins, Channel, Type, ID, NewAlerts, OtherAlerts, Decay) ->
    lager:warning("~s ~s ~p ~p", [Type, ID, NewAlerts, OtherAlerts]),
    Message = [
        if
            length(NewAlerts) > 0 ->
                [" just raised", stringify_alerts(NewAlerts)];
            true ->
                []
        end,
        if
            length(OtherAlerts) > 0 ->
                [" and still", stringify_alerts(OtherAlerts)];
            true ->
                []
        end],
    Warning = Decay > 85,
    % send e-mails to admins
    send_emails(Admins, Type, ID, Message, Warning),
    % send a notification to slack
    if
        is_binary(Channel), byte_size(Channel) > 0 ->
            send_slack(Channel, Type, ID, Message, Warning);
        true ->
            ok
    end.

stringify_url(Prefix, Type, ID) ->
    io_lib:format("~s#/~ss/edit/~s", [Prefix, Type, ID]).

stringify_alerts(Alerts) ->
    [io_lib:format(" ~s (~B%)", [Alert, Decay]) || {Alert, Decay} <- Alerts].

send_emails(undefined, _Type, _ID, _Message, _Warning) ->
    ok;
send_emails(Admins, Type, ID, Message, Warning) ->
    send_emails0(
        lists:filtermap(
            fun(User) ->
                case mnesia:dirty_read(user, User) of
                    [#user{email=EMail, send_alerts=true}]
                            when is_binary(EMail), byte_size(EMail) > 0 ->
                        {true, EMail};
                    _Else ->
                        false
                end
            end, Admins),
        Type, ID, Message, Warning).

send_emails0([], _Type, _ID, _Message, _Warning) ->
    ok;
send_emails0(ToAddrs, Type, ID, Message, Warning) ->
    [#config{admin_url=Prefix, email_from=From, email_server=Server,
        email_user=User, email_password=Pass}] = mnesia:dirty_read(config, <<"main">>),
    TypeTitle = titlecase(Type),
    Body =
        {<<"multipart">>, <<"alternative">>, [
            {<<"From">>, From},
            {<<"To">>, lists:join(<<";">>, ToAddrs)},
            {<<"Subject">>, list_to_binary([TypeTitle, " ", ID])},
            {<<"MIME-Version">>, <<"1.0">>},
            {<<"Content-Type">>, <<"multipart/alternative; boundary=bound-123234234">>}] ++
            if
                Warning ->
                    [{<<"Importance">>, <<"high">>},
                     {<<"X-Priority">>, <<"1">>}];
                true ->
                    []
            end,
        [{<<"content-type-params">>, [{<<"boundary">>, <<"bound-123234234">>}]},
            {<<"disposition">>,<<"inline">>},
            {<<"disposition-params">>,[]}],
        [{<<"text">>,<<"plain">>,
            [{<<"Content-Type">>, <<"text/plain;charset=US-ASCII">>},
                {<<"Content-Transfer-Encoding">>,<<"7bit">>}],
            [{<<"content-type-params">>, [{<<"charset">>,<<"US-ASCII">>}]},
                {<<"disposition">>,<<"inline">>}],
            list_to_binary([TypeTitle, " ", ID, Message])},
        {<<"text">>,<<"html">>,
            [{<<"Content-Type">>,<<"text/html;charset=US-ASCII">>},
                {<<"Content-Transfer-Encoding">>,<<"7bit">>}],
            [{<<"content-type-params">>, [{<<"charset">>,<<"US-ASCII">>}]},
                {<<"disposition">>,<<"inline">>}],
            list_to_binary(["<html><body>", TypeTitle,
                " <a href=\"", stringify_url(Prefix, Type, ID), "\">", ID, "</a>", Message, "</body></html>"])}]},
    gen_smtp_client:send({From, ToAddrs, mimemail:encode(Body)},
        if
            is_binary(Server), byte_size(Server) > 0 ->
                [{relay, Server}];
            true ->
                []
        end ++
        if
            is_binary(User), byte_size(User) > 0 ->
                [{username, User}];
            true ->
                []
        end ++
        if
            is_binary(Pass), byte_size(Pass) > 0 ->
                [{password, Pass}];
            true ->
                []
        end).

send_slack(Channel, Type, ID, Message, Warning) ->
    case catch send_slack_message(Channel, Type, ID, Message, Warning) of
        true ->
            ok;
        Else ->
            lager:error("Cannot send Slack message: ~p", [Else])
    end.

send_slack_message(Channel, Type, ID, Message, Warning) ->
    [#config{admin_url=Prefix, slack_token=Token}] = mnesia:dirty_read(config, <<"main">>),
    send_slack_raw(
        Token, Channel,
        list_to_binary([titlecase(Type), " <", stringify_url(Prefix, Type, ID), "|", ID, ">", Message,
            if
                Warning ->
                    " :warning:";
                true ->
                    []
            end])).

send_slack_raw(undefined, _Channel, _Message) ->
    {error, token_undefined};
send_slack_raw(Token, Channel, Message) ->
    {ok, {Host, Port}} = application:get_env(lorawan_server, slack_server),
    Opts = application:get_env(lorawan_server, ssl_options, []),
    {ok, ConnPid} = gun:open(Host, Port, #{transport=>ssl, transport_opts=>Opts}),
    {ok, _} = gun:await_up(ConnPid),
    Payload = #{
        channel => Channel,
        text => Message,
        as_user => true},
    StreamRef = gun:post(ConnPid, <<"/api/chat.postMessage">>,
        [{<<"content-type">>, <<"application/json">>},
        {<<"authorization">>, <<"Bearer ", Token/binary>>}],
        jsx:encode(Payload)),
    {response, nofin, 200, _Headers} = gun:await(ConnPid, StreamRef),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    ok = gun:close(ConnPid),
    Result = jsx:decode(Body, [return_maps]),
    maps:get(<<"ok">>, Result, false).

check_alive(#gateway{last_alive=undefined}) ->
    {<<"disconnected">>, 100};
check_alive(#gateway{last_alive=LastAlive}) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case Now - calendar:datetime_to_gregorian_seconds(LastAlive) of
        Silent when Silent > 1500 ->
            {<<"disconnected">>, 100};
        Silent when Silent > 30 ->
            Next = calendar:gregorian_seconds_to_datetime(Now + 30 - Silent rem 30),
            {<<"disconnected">>, 2*(Silent div 30), Next};
        _Else ->
            Next = calendar:gregorian_seconds_to_datetime(Now + 30),
            {ok, Next}
    end.

check_dwell(#gateway{dwell=Dwell}) when is_list(Dwell) ->
    MaxSum =
        lists:foldl(
            fun({_, {_, _, Sum}}, Max) -> max(Sum, Max) end,
            0, Dwell),
    Percent = MaxSum/36000,
    Next = calendar:gregorian_seconds_to_datetime(
        30 + calendar:datetime_to_gregorian_seconds(calendar:universal_time())),
    if
        Percent > 1 ->
            {<<"dwell_time_violated">>, trunc(10*Percent), Next};
        true ->
            {ok, Next}
    end;
check_dwell(_Other) ->
    ok.

check_reset(#node{last_reset=LastRes, reset_count=Count, last_rx=LastRx})
        when LastRes /= undefined, is_number(Count), Count > 1, (LastRx == undefined orelse LastRes > LastRx) ->
    {<<"many_resets">>, 20*(Count-1)};
check_reset(#node{}) ->
    ok.

check_battery(#node{devstat=[{_Time, Battery, _Margin, _MaxSNR}|_]}) ->
    if
        Battery == 0 ->
            % connected to external power
            ok;
        Battery < 100 ->
            % TODO: should estimate trend instead
            {<<"battery_low">>, 100-Battery};
        Battery == 255 ->
            {<<"cannot_measure_battery">>, 25};
        true ->
            ok
    end;
check_battery(#node{}) ->
    undefined.

check_margin(#node{devstat=DevStat}) when is_list(DevStat), length(DevStat) > 1 ->
    % last (two or) three values
    Deltas =
        lists:map(
            fun({_Time, _Battery, Margin, MaxSNR}) -> Margin - MaxSNR end,
            lists:sublist(DevStat, 3)),
    % if all three are bad, rank the minimum
    Threshold = 10,
    case lists:all(
            fun(Delta) -> Delta < Threshold end, Deltas) of
        true ->
            {<<"downlink_noise">>, max(100, trunc(5*(Threshold-lists:min(Deltas))))};
        false ->
            ok
    end;
check_margin(#node{}) ->
    ok.

check_adr(#node{adr_failed=Failed}) when Failed == undefined; Failed == [] ->
    ok;
check_adr(#node{}) ->
    {<<"linkadr_failed">>, 25}.

check_rxwin(#node{rxwin_failed=Failed}) when Failed == undefined; Failed == [] ->
    ok;
check_rxwin(#node{}) ->
    {<<"rxparamsetup_failed">>, 25}.

check_failed(#connector{enabled=true, failed=Empty}) when Empty == undefined; Empty == [] ->
    ok;
check_failed(#connector{enabled=true, failed=[_|_]}) ->
    {<<"failed">>, 100};
check_failed(#connector{enabled=false}) ->
    undefined.

node_deleted(DevAddr) ->
    lager:debug("Node ~p deleted", [lorawan_utils:binary_to_hex(DevAddr)]),
    % delete linked records
    ok = mnesia:dirty_delete(pending, DevAddr),
    delete_matched(queued, #queued{frid='$1', devaddr=DevAddr, _='_'}),
    delete_matched(rxframe, #rxframe{frid='$1', devaddr=DevAddr, _='_'}).

delete_matched(Table, Record) ->
    lists:foreach(
        fun(Id) ->
            ok = mnesia:dirty_delete(Table, Id)
        end,
        mnesia:dirty_select(Table, [{Record, [], ['$1']}])).

trim_rxframes() ->
    {ok, Count} = application:get_env(lorawan_server, retained_rxframes),
    Trimmed = lists:filter(
        fun(D) ->
            {Uplinks, Downlinks} = lorawan_db:get_rxframes(D),
            % trim uplinks and downlinks separately
            trim_rxframes(Uplinks, Count) or trim_rxframes(Downlinks, Count)
        end,
        lists:usort(
            mnesia:dirty_select(rxframe, [{#rxframe{devaddr='$1', _='_'}, [], ['$1']}]))),
    % log message
    if
        length(Trimmed) > 0 ->
            lager:debug("Expired rxframes from ~p",
                [[lorawan_utils:binary_to_hex(E) || E <- Trimmed]]);
        true ->
            ok
    end.

trim_rxframes(Frames, Count) when length(Frames) > Count ->
    lists:foreach(
        fun(R) -> mnesia:dirty_delete_object(rxframe, R) end,
        lists:sublist(Frames, length(Frames)-Count)),
    true;
trim_rxframes(_Frames, _Count) ->
    % nothing to trim
    false.

purge_queued(DevAddr) ->
    lists:foreach(
        fun(Obj) ->
            ok = mnesia:dirty_delete_object(queued, Obj)
        end,
        mnesia:dirty_match_object(queued, #queued{devaddr=DevAddr, _='_'})).

expired_events() ->
    {ok, AgeSeconds} = application:get_env(lorawan_server, event_lifetime),
    ETime = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - AgeSeconds),
    mnesia:dirty_select(event,
        [{#event{evid='$1', last_rx='$2', _='_'}, [{'=<', '$2', {const, ETime}}], ['$1']}]).

% string:titlecase is not available in Erlang 19
titlecase([F|Rest]) ->
    [string:to_upper(F) | Rest].

% end of file
