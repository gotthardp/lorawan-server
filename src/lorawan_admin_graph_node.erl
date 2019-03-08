%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_graph_node).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2, generate_etag/2]).

-export([get_rxframe/2]).

-include("lorawan_db.hrl").
-record(state, {scopes, auth_fields}).

init(Req, Scopes) ->
    {cowboy_rest, Req, #state{scopes=Scopes}}.

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
        {{<<"application">>, <<"json">>, []}, get_rxframe}
    ], Req, State}.

get_rxframe(Req, State) ->
    DevAddr = cowboy_req:binding(devaddr, Req),
    [Node] = mnesia:dirty_read(node, lorawan_utils:hex_to_binary(DevAddr)),
    Req2 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-store">>, Req),
    {jsx:encode([{devaddr, DevAddr}, {array, get_array(Node)}]), Req2, State}.

get_array(#node{last_reset=Reset, devstat=DevStat}) when is_list(DevStat) ->
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    [{cols, [
        [{id, <<"timestamp">>}, {label, <<"Timestamp">>}, {type, <<"datetime">>}],
        [{type, <<"string">>}, {role, <<"annotation">>}],
        [{id, <<"batt">>}, {label, <<"Battery">>}, {type, <<"number">>}],
        [{id, <<"snr">>}, {label, <<"D/L SNR (dB)">>}, {type, <<"number">>}],
        [{id, <<"max_snr">>}, {label, <<"Max SNR (dB)">>}, {type, <<"number">>}]
    ]},
    {rows,
        [[{c, [
            [{v, lorawan_admin:timestamp_to_json_date(Reset)}],
            [{v, <<"Last Reset">>}],
            [{v, null}],
            [{v, null}],
            [{v, null}]
        ]}] | lists:map(
        fun({Timestamp, Batt, Margin, MaxSNR}) ->
            [{c, [
                [{v, lorawan_admin:timestamp_to_json_date(Timestamp)}],
                [{v, null}],
                [{v, Batt}],
                % what the standard calls "margin" is simply the SNR
                [{v, Margin}],
                [{v, MaxSNR}]
            ]}];
        % backwards compatibility
        % REMOVE BEFORE RELEASING 0.4.11
        ({_Timestamp, _Batt, _Margin}) ->
            [{c, []}]
        end, DevStat)]
    }];
get_array(_Else) ->
    [].

resource_exists(Req, State) ->
    case mnesia:dirty_read(node,
            lorawan_admin:parse_field(devaddr, cowboy_req:binding(devaddr, Req))) of
        [] -> {false, Req, State};
        [_Node] -> {true, Req, State}
    end.

generate_etag(Req, State) ->
    DevAddr = cowboy_req:binding(devaddr, Req),
    case mnesia:dirty_read(node, lorawan_utils:hex_to_binary(DevAddr)) of
        [] ->
            {undefined, Req, State};
        [#node{last_reset=Reset, devstat=DevStat}] ->
            Hash = base64:encode(crypto:hash(sha256, term_to_binary({Reset, DevStat}))),
            {<<$", Hash/binary, $">>, Req, State}
    end.

% end of file
