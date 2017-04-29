%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%

-record(stat, {
    time, lati, long, alti, rxnb, rxok, rxfw, ackr, dwnb, txnb,
    mail, desc % TTN extensions
}).

-record(ignored_link, {devaddr, mask}).
-record(connector, {connid, enabled, uri, published, subscribe, consumed, client_id, auth, name, pass, certfile, keyfile}).
-record(handler, {appid, format, parse, build, connid}).
-record(event, {evid, severity, count, datetime, entity, eid, text}).

-define(to_record(Record, List, Default),
    list_to_tuple([Record|[proplists:get_value(X, List, Default) || X <- record_info(fields, Record)]])).

-define(to_record(Record, List), ?to_record(Record, List, undefined)).

-define(to_proplist(Record, RecData),
    lists:filter(fun({_, undefined}) -> false;
                    (_) -> true
                end,
        lists:zip(record_info(fields, Record), tl(tuple_to_list(RecData)))
    )).

% end of file
