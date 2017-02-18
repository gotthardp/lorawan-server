%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%

-record(rxq, {freq, datr, codr, time, tmst, erlst, chan, rfch, stat, rssi, lsnr}).
-record(txq, {freq, datr, codr, tmst, rfch, powe}).
-record(stat, {time, lati, long, alti, rxnb, rxok, rxfw, ackr, dwnb, txnb}).

-record(ignored_link, {devaddr, mask}).

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
