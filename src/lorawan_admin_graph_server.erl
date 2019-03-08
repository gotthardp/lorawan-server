%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_graph_server).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_server/2]).

-include("lorawan.hrl").
-record(state, {key, scopes, auth_fields}).

init(Req, Scopes) ->
    Key = lorawan_admin:parse_field(sname, cowboy_req:binding(sname, Req)),
    {cowboy_rest, Req, #state{key=Key, scopes=Scopes}}.

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
        {{<<"application">>, <<"json">>, []}, get_server}
    ], Req, State}.

get_server(Req, #state{key=Key}=State) ->
    Server = mnesia:dirty_read(server, Key),
    {jsx:encode([{name, Key}, {array, get_array(Server)}]), Req, State}.

get_array([#server{router_perf=Perf}]) when is_list(Perf) ->
    [{cols, [
        [{id, <<"timestamp">>}, {label, <<"Timestamp">>}, {type, <<"datetime">>}],
        [{id, <<"requests">>}, {label, <<"Requests per min">>}, {type, <<"number">>}],
        [{id, <<"errors">>}, {label, <<"Errors per min">>}, {type, <<"number">>}]
    ]},
    {rows, lists:map(
        fun ({Date, {ReqCnt, ErrCnt}}) ->
            [{c, [
                [{v, lorawan_admin:timestamp_to_json_date(Date)}],
                [{v, ReqCnt}],
                [{v, ErrCnt}]
            ]}]
        end, Perf)
    }];
get_array(_Else) ->
    [].

resource_exists(Req, #state{key=Key}=State) ->
    case mnesia:dirty_read(server, Key) of
        [] -> {false, Req, State};
        [_] -> {true, Req, State}
    end.

% end of file
