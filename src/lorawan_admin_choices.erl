%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_choices).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([handle_get/2]).

-include("lorawan_db.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, Opts) ->
    lorawan_admin:handle_authorization(Req, Opts).

allowed_methods(Req, Opts) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, Opts}.

content_types_provided(Req, Opts) ->
    {[
        {{<<"application">>, <<"json">>, []}, handle_get}
    ], Req, Opts}.

handle_get(Req, networks=Opts) ->
    Nets =
        lists:map(
            fun(Net) ->
                {Net, network_choices(Net)}
            end,
            mnesia:dirty_all_keys(networks)),
    {jsx:encode(Nets), Req, Opts};
handle_get(Req, profiles=Opts) ->
    Profs =
        lists:map(
            fun(Prof) ->
                [#profile{network=Net}] = mnesia:dirty_read(profiles, Prof),
                {Prof, network_choices(Net)}
            end,
            mnesia:dirty_all_keys(profiles)),
    {jsx:encode(Profs), Req, Opts}.

network_choices(Net) ->
    [#network{region=Region, max_eirp=Max, min_eirp=Min}] = mnesia:dirty_read(networks, Net),
    [
        {data_rate, data_rate_choices0(Region)},
        {power, power_choices0(0, Max, Min)}
    ].

data_rate_choices0(Region) ->
    if
        Region == <<"EU868">>; Region == <<"CN779">>; Region == <<"EU433">>;
        Region == <<"CN470">>; Region == <<"KR920">>; Region == <<"AS923-JP">> -> [
            [{value, 0}, {label, <<"SF12 125 kHz (250 bit/s)">>}],
            [{value, 1}, {label, <<"SF11 125 kHz (440 bit/s)">>}],
            [{value, 2}, {label, <<"SF10 125 kHz (980 bit/s)">>}],
            [{value, 3}, {label, <<"SF9 125 kHz (1760 bit/s)">>}],
            [{value, 4}, {label, <<"SF8 125 kHz (3125 bit/s)">>}],
            [{value, 5}, {label, <<"SF7 125 kHz (5470 bit/s)">>}]] ++
        if
            Region == <<"EU868">>; Region == <<"CN779">>; Region == <<"EU433">> -> [
                [{value, 6}, {label, <<"SF7 250 kHz (11000 bit/s)">>}],
                [{value, 7}, {label, <<"50 kbps (50000 bit/s)">>}]];
            true ->
                []
        end;
        Region == <<"US902">>; Region == <<"US902-PR">>; Region == <<"AU915">> -> [
            [{value, 0}, {label, <<"SF10 125 kHz (980 bit/s)">>}],
            [{value, 1}, {label, <<"SF9 125 kHz (1760 bit/s)">>}],
            [{value, 2}, {label, <<"SF8 125 kHz (3125 bit/s)">>}],
            [{value, 3}, {label, <<"SF7 125 kHz (5470 bit/s)">>}],
            [{value, 4}, {label, <<"SF8 500 kHz (12500 bit/s)">>}]]
    end.

power_choices0(Idx, Power, Min) when Idx =< Min ->
    [[{value, Idx}, {label, <<(integer_to_binary(Power))/binary, " dBm">> }] | power_choices0(Idx+1, Power-2, Min)];
power_choices0(_Idx, _Power, _Min) ->
    [].

resource_exists(Req, State) ->
    {true, Req, State}.

% end of file
