% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_external_attachment).
-export([att_foldl/3]).

-define(BUFFER_SIZE, 16384).

att_foldl(#att{location={external, Location},md5=Md5}, Fun, Acc) ->
    {ok, Fd} = file:open(resolve_location(Location), [read, raw, binary]),
    try
        copy_loop(Fd, Fun, Acc)
    after
        ok = file:close(Fd)
    end.

copy_loop(Fd, Fun, Acc) ->
    case file:read(Fd, ?BUFFER_SIZE) of
        {ok, Bin} ->
            copy_loop(Fd, Fun, Fun(Bin, Acc));
        eof ->
            Acc
    end.

resolve_location(Location) ->
    RootDir = couch_config:get("couchdb", "attachment_dir", "."),
    RootDir ++ Location.
