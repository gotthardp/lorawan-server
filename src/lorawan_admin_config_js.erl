%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_config_js).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).

-export([handle_get/2]).

-include("lorawan.hrl").
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
        {{<<"application">>, <<"javascript">>, []}, handle_get}
    ], Req, State}.

handle_get(Req, State) ->
    [#config{items_per_page=Items, google_api_key=GMaps}] =
        mnesia:dirty_read(config, <<"main">>),
    {variable(<<"NodeName">>, atom_to_binary(node(), latin1),
     variable(<<"GoogleAPIKey">>, GMaps,
        if
            Items == undefined ->
                variable(<<"ItemsPerPage">>, 30,
                variable(<<"InfinitePagination">>, true, <<>>));
            true ->
                variable(<<"ItemsPerPage">>, Items,
                variable(<<"InfinitePagination">>, false, <<>>))
        end)), Req, State}.

variable(Name, undefined, Bin) ->
    <<"var ", Name/binary, "=null;\r", Bin/binary>>;
variable(Name, Value, Bin) when is_integer(Value) ->
    <<"var ", Name/binary, "=", (integer_to_binary(Value))/binary, ";\r", Bin/binary>>;
variable(Name, Value, Bin) when is_binary(Value) ->
    <<"var ", Name/binary, "=\"", Value/binary, "\";\r", Bin/binary>>;
variable(Name, true, Bin) ->
    <<"var ", Name/binary, "=true;\r", Bin/binary>>;
variable(Name, false, Bin) ->
    <<"var ", Name/binary, "=false;\r", Bin/binary>>.

% end of file
