%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% Simplified version of cowboy_static with authentication and gzip.
%
-module(lorawan_admin_static).

-export([init/2]).
-export([is_authorized/2]).
-export([malformed_request/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([last_modified/2]).
-export([generate_etag/2]).
-export([get_file/2]).

-include_lib("kernel/include/file.hrl").

init(Req, {priv_file, App, Path}) ->
    init_rest(Req, [priv_dir(App), Path]);
init(Req, {priv_dir, App, Path}) ->
    init_rest(Req, [priv_dir(App), Path | cowboy_req:path_info(Req)]).

priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name} ->
            lager:error("Unknown priv_dir of ~p", [App]),
            [];
        PrivDir ->
            PrivDir
    end.

init_rest(Req, Path0) ->
    {cowboy_rest, Req, init_status(Req, Path0)}.

init_status(Req, Path0) ->
    Path = filename:absname(filename:join(Path0)),
    case gzip_accepted(Req) of
        true ->
            Path2 = <<Path/binary, ".gz">>,
            case file:read_file_info(Path2, [{time, universal}]) of
                {ok, Info} ->
                    {Path2, Info, cow_mimetypes:all(Path)};
                {error, _} ->
                    init_status_plain(Path)
            end;
        false ->
            init_status_plain(Path)
    end.

gzip_accepted(Req) ->
    case cowboy_req:parse_header(<<"accept-encoding">>, Req) of
        undefined ->
            false;
        Encodings ->
            case [E || E={<<"gzip">>, Q} <- Encodings, Q =/= 0] of
                [] ->
                    false;
                _ ->
                    true
            end
    end.

init_status_plain(Path) ->
    case file:read_file_info(Path, [{time, universal}]) of
        {ok, Info} -> {Path, Info, undefined};
        Error -> Error
    end.

is_authorized(Req, State) ->
    {lorawan_admin:handle_authorization(Req), Req, State}.

malformed_request(Req, State) ->
    {State =:= error, Req, State}.

forbidden(Req, State={error, _}) ->
    {true, Req, State};
forbidden(Req, State={_, #file_info{type=directory}, _}) ->
    {true, Req, State};
forbidden(Req, State={_, #file_info{access=Access}, _})
        when Access =:= write; Access =:= none ->
    {true, Req, State};
forbidden(Req, State) ->
    {false, Req, State}.

content_types_provided(Req, State={Path, _, _}) ->
    {[{cow_mimetypes:web(Path), get_file}], Req, State}.

resource_exists(Req, State={_, #file_info{type=regular}, _}) ->
    {true, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.

generate_etag(Req, State={_, #file_info{size=Size, mtime=Mtime}, _}) ->
    {generate_default_etag(Size, Mtime), Req, State}.

generate_default_etag(Size, Mtime) ->
    {strong, integer_to_binary(erlang:phash2({Size, Mtime}, 16#ffffffff))}.

last_modified(Req, State={_, #file_info{mtime=Modified}, _}) ->
    {Modified, Req, State}.

get_file(Req, State={Path, #file_info{size=Size}, Compressed}) ->
    Req2 =
        case Compressed of
            {Type, Subtype, []} ->
                cowboy_req:set_resp_header(<<"content-type">>, <<Type/binary, $/, Subtype/binary>>,
                    cowboy_req:set_resp_header(<<"content-encoding">>, <<"gzip">>, Req));
            undefined ->
                Req
        end,
    {{sendfile, 0, Size, Path}, Req2, State}.

% end of file
