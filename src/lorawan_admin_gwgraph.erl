%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_gwgraph).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_gateway/2]).

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
        {{<<"application">>, <<"json">>, []}, get_gateway}
    ], Req, State}.

get_gateway(Req, State) ->
    MAC = cowboy_req:binding(mac, Req),
    [#gateway{delays=Delays}] = mnesia:dirty_read(gateways, lorawan_mac:hex_to_binary(MAC)),
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    Array = [{cols, [
                [{id, <<"timestamp">>}, {label, <<"Timestamp">>}, {type, <<"datetime">>}],
                [{id, <<"delay">>}, {label, <<"Delay [ms]">>}, {type, <<"number">>}]
                ]},
            {rows, lists:filtermap(
                fun ({Date, Delay}) ->
                    {true,  [{c, [
                                [{v, encode_timestamp(Date)}],
                                [{v, Delay}]
                            ]}]}
                end, Delays)
            }
        ],
    {jsx:encode([{mac, MAC}, {array, Array}]), Req, State}.

encode_timestamp({{Yr,Mh,Dy},{Hr,Me,Sc}}) ->
    list_to_binary(
        lists:concat(["Date(",
            integer_to_list(Yr), ",", integer_to_list(Mh), ",", integer_to_list(Dy), ",",
            integer_to_list(Hr), ",", integer_to_list(Me), ",", integer_to_list(Sc), ")"])).

resource_exists(Req, State) ->
    case mnesia:dirty_read(gateways,
            lorawan_mac:hex_to_binary(cowboy_req:binding(mac, Req))) of
        [] -> {false, Req, State};
        [#gateway{delays=undefined}] -> {false, Req, State};
        [_Gateway] -> {true, Req, State}
    end.

% end of file
