%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_upload).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_accepted/2]).

-export([handle_write/2]).

-record(state, {scopes, auth_fields}).

init(Req, Scopes) ->
    {cowboy_rest, Req, #state{scopes=Scopes}}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"POST">>], Req, State}.

is_authorized(Req, #state{scopes=Scopes}=State) ->
    case lorawan_admin:handle_authorization(Req, Scopes) of
        {true, AuthFields} ->
            {true, Req, State#state{auth_fields=AuthFields}};
        Else ->
            {Else, Req, State}
    end.

forbidden(Req, #state{auth_fields=AuthFields}=State) ->
    {lorawan_admin:fields_empty(AuthFields), Req, State}.

content_types_accepted(Req, State) ->
    {[
        {'*', handle_write}
    ], Req, State}.

handle_write(Req, State) ->
    {ok, Headers, Req2} = cowboy_req:read_part(Req),
    {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
    {file, <<"file">>, FileName, ContentType} = cow_multipart:form_data(Headers),
    case file:write_file(FileName, Data) of
        ok ->
            lager:debug("Uploaded ~p of content-type ~p", [FileName, ContentType]),
            {true, Req3, State};
        {error, Error} ->
            lager:error("Cannot upload ~p of content-type ~p: ~p", [FileName, ContentType, Error]),
            {stop, cowboy_req:reply(400, Req3), State}
    end.

% end of file
