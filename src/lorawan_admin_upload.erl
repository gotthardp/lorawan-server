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
    {file, <<"file">>, FileName, ContentType, _TE} = cow_multipart:form_data(Headers),
    lager:debug("Upload ~p of content-type ~p", [FileName, ContentType]),
    file:write_file(FileName, Data),
    {ok, Req3, Opts}.

% end of file
