%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_devstat).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_rxframe/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_rxframe}
    ], Req, State}.

get_rxframe(Req, State) ->
    DevAddr = cowboy_req:binding(devaddr, Req),
    [#link{devstat=DevStat}] = mnesia:dirty_read(links, lorawan_mac:hex_to_binary(DevAddr)),
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    Array = [{cols, [
                [{id, <<"timestamp">>}, {label, <<"Timestamp">>}, {type, <<"datetime">>}],
                [{id, <<"batt">>}, {label, <<"Battery">>}, {type, <<"number">>}],
                [{id, <<"snr">>}, {label, <<"D/L SNR (dB)">>}, {type, <<"number">>}],
                [{id, <<"max_snr">>}, {label, <<"Max SNR (dB)">>}, {type, <<"number">>}]
                ]},
            {rows, lists:map(
                fun({Timestamp, Batt, Margin, MaxSNR}) ->
                    [{c, [
                        [{v, encode_timestamp(Timestamp)}],
                        [{v, Batt}],
                        % what the standard calls "margin" is simply the SNR
                        [{v, Margin}],
                        [{v, MaxSNR}]
                    ]}];
                % backwards compatibility
                % REMOVE BEFORE RELEASING 0.4.11
                ({Timestamp, Batt, Margin}) ->
                    [{c, []}]
                end, DevStat)
            }
        ],
    {jsx:encode([{devaddr, DevAddr}, {array, Array}]), Req, State}.

encode_timestamp({{Yr,Mh,Dy},{Hr,Me,Sc}}) ->
    list_to_binary(
        lists:concat(["Date(",
            integer_to_list(Yr), ",", integer_to_list(Mh), ",", integer_to_list(Dy), ",",
            integer_to_list(Hr), ",", integer_to_list(Me), ",", integer_to_list(Sc), ")"])).

resource_exists(Req, State) ->
    case mnesia:dirty_read(links,
            lorawan_mac:hex_to_binary(cowboy_req:binding(devaddr, Req))) of
        [#link{devstat=DevStat}] when is_list(DevStat) ->
            {true, Req, State};
        _Else ->
            {false, Req, State}
    end.

% end of file
