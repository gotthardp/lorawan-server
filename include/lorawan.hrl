%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%

-record(stat, {
    time, lati, long, alti, rxnb, rxok, rxfw, ackr, dwnb, txnb,
    mail, desc % TTN extensions
}).

-record(frame, {conf, devaddr, adr, adr_ack_req, ack, fcnt, fopts, port, data}).

-define(to_record(Record, Object, Default),
    list_to_tuple([Record|[maps:get(X, Object, Default) || X <- record_info(fields, Record)]])).

-define(to_record(Record, Object), ?to_record(Record, Object, undefined)).

-define(to_map(Record, RecData),
    maps:from_list(
        lists:filtermap(
            fun ({_K, D, D}) -> false;
                ({K, V, _D}) -> {true, {K, V}}
            end,
            lists:zip3(record_info(fields, Record), lorawan_db:record_fields(RecData), tl(tuple_to_list(#Record{})))
    ))).

-define(REALM, <<"lorawan-server">>).

-record(user, {
    name :: nonempty_string(),
    pass_ha1 :: string(),
    roles :: [string()]}).

-record(server, {
    sname :: atom(),
    router_perf :: [{calendar:datetime(), {integer(), integer()}}],
    log_ignored :: boolean()}).

-record(event, {
    evid :: binary(),
    severity :: atom(),
    first_rx :: calendar:datetime(),
    last_rx :: calendar:datetime(),
    count :: integer(),
    entity :: atom(),
    eid :: binary(),
    text :: binary(),
    args :: 'undefined' | binary()}).

% end of file
