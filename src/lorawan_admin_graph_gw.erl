%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_graph_gw).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_gateway/2]).

-include("lorawan_db.hrl").
-record(state, {format, scopes, auth_fields}).

init(Req, {Format, Scopes}) ->
    {cowboy_rest, Req, #state{format=Format, scopes=Scopes}}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

is_authorized(Req, #state{scopes=Scopes}=State) ->
    case lorawan_admin:handle_authorization(Req, Scopes) of
        {true, AuthFields} ->
            {true, Req, State#state{auth_fields=AuthFields}};
        Else ->
            {Else, Req, State}
    end.

forbidden(Req, #state{auth_fields=AuthFields}=State) ->
    {lorawan_admin:fields_empty(AuthFields), Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_gateway}
    ], Req, State}.

get_gateway(Req, #state{format=Type}=State) ->
    MAC = cowboy_req:binding(mac, Req),
    [Gateway] = mnesia:dirty_read(gateway, lorawan_utils:hex_to_binary(MAC)),
    {jsx:encode([{mac, MAC}, {array, get_array(Type, Gateway)}]), Req, State}.

get_array(pgraph, #gateway{delays=Delays}) when is_list(Delays) ->
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    [{cols, [
        [{id, <<"timestamp">>}, {label, <<"Timestamp">>}, {type, <<"datetime">>}],
        [{id, <<"avgdelay">>}, {label, <<"Average [ms]">>}, {type, <<"number">>}],
        [{type, <<"number">>}, {role, <<"interval">>}],
        [{type, <<"number">>}, {role, <<"interval">>}]
    ]},
    {rows, lists:filtermap(
        fun ({Date, {Min, Avg, Max}}) ->
            {true,  [{c, [
                        [{v, lorawan_admin:timestamp_to_json_date(Date)}],
                        [{v, Avg}],
                        [{v, Min}],
                        [{v, Max}]
                    ]}]};
        (_Else) ->
            false
        end, Delays)
    }];
get_array(tgraph, #gateway{dwell=Dwell}) when is_list(Dwell) ->
    [{cols, [
        [{id, <<"timestamp">>}, {label, <<"Timestamp">>}, {type, <<"datetime">>}],
        [{id, <<"dwell">>}, {label, <<"Tx Time [ms]">>}, {type, <<"number">>}],
        [{id, <<"sum">>}, {label, <<"Tx in Hour [ms]">>}, {type, <<"number">>}]
    ]},
    {rows, lists:filtermap(
        fun ({Date, {_, Duration, Sum}}) ->
            {true,  [{c, [
                        [{v, lorawan_admin:timestamp_to_json_date(Date)}],
                        [{v, if Duration == 0 -> null; true -> round(Duration) end}],
                        [{v, Sum/36000}, {f, <<(integer_to_binary(round(Sum)))/binary,
                            " (", (float_to_binary(Sum/36000, [{decimals, 3}, compact]))/binary, "%)">>}]
                    ]}]};
        (_Else) ->
            false
        end, Dwell)
    }];
get_array(_, _Else) ->
    [].

resource_exists(Req, State) ->
    case mnesia:dirty_read(gateway,
            lorawan_admin:parse_field(mac, cowboy_req:binding(mac, Req))) of
        [] -> {false, Req, State};
        [_Stats] -> {true, Req, State}
    end.

% end of file
