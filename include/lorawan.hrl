%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%

-record(rxq, {time, tmst, chan, rfch, stat, rssi, lsnr}).
-record(txq, {tmst, rfch, powe}).
-record(rflora, {freq, datr, codr}).
-record(stat, {time, lati, long, alti, rxnb, rxok, rxfw, ackr, dwnb, txnb}).

-record(user, {name, pass}).
-record(gateway, {mac, netid, gpspos, gpsalt}).
-record(device, {deveui, app, appid, appeui, appkey, link}).
-record(link, {devaddr, app, appid, nwkskey, appskey, fcntup, fcntdown}).
-record(rxframe, {devcnt, macrxq, rflora, data}).

-define(to_record(Record, List),
    list_to_tuple([Record|[proplists:get_value(X, List) || X <- record_info(fields, Record)]])).

-define(to_proplist(Record, RecData),
    lists:filter(fun({_, undefined}) -> false;
                    (_) -> true
                end,
        lists:zip(record_info(fields, Record), tl(tuple_to_list(RecData)))
    )).

% end of file
