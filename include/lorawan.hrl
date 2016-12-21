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
-record(device, {deveui, app, appid, appeui, appkey, link, adr_flag_set, adr_set}).
-record(link, {devaddr, app, appid, nwkskey, appskey, fcntup, fcntdown,
    adr_flag_use, adr_flag_set, adr_use, adr_set}).
-record(pending, {devaddr, phypayload}).
-record(txframe, {frid, datetime, devaddr, txdata}).
-record(rxframe, {frid, mac, rssi, lsnr, freq, datr, codr, devaddr, fcnt}).

-record(rxdata, {port, data, last_lost=false, shall_reply=false}).
-record(txdata, {confirmed=false, port, data, pending=false}).

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
