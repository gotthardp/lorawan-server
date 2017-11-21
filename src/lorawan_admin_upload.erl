%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_upload).

-export([init/2]).

init(Req, Opts) ->
    {ok, Headers, Req2} = cowboy_req:read_part(Req),
    {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
    {file, <<"file">>, FileName, ContentType} = cow_multipart:form_data(Headers),
    case file:write_file(FileName, Data) of
        ok ->
            lager:debug("Uploaded ~p of content-type ~p", [FileName, ContentType]),
            {ok, Req3, Opts};
        {error, Error} ->
            lager:error("Cannot upload ~p of content-type ~p: ~p", [FileName, ContentType, Error]),
            Req4 = cowboy_req:reply(400, Req3),
            {ok, Req4, Opts}
    end.

% end of file
