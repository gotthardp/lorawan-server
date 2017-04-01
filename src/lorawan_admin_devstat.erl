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
    ActRec = lorawan_db:get_rxframes(lorawan_mac:hex_to_binary(DevAddr)),
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    Array = [{cols, [
                [{id, <<"fcnt">>}, {label, <<"FCnt">>}, {type, <<"number">>}],
                [{id, <<"batt">>}, {label, <<"Battery">>}, {type, <<"number">>}],
                [{id, <<"snr">>}, {label, <<"SNR (dB)">>}, {type, <<"number">>}]
                ]},
            {rows, lists:filtermap(
                fun (#rxframe{devstat=undefined}) ->
                    false;
                (#rxframe{fcnt=FCnt, devstat={Batt, Margin}}) ->
                    {true,  [{c, [
                                [{v, FCnt}],
                                [{v, Batt}],
                                % what the standard calls "margin" is simply the SNR
                                [{v, Margin}]
                            ]}]}
                end, ActRec)
            }
        ],
    {jsx:encode([{devaddr, DevAddr}, {array, Array}]), Req, State}.

resource_exists(Req, State) ->
    case mnesia:dirty_index_read(rxframes,
            lorawan_mac:hex_to_binary(cowboy_req:binding(devaddr, Req)), #rxframe.devaddr) of
        [] -> {false, Req, State};
        [_First|_Rest] -> {true, Req, State}
    end.

% end of file
