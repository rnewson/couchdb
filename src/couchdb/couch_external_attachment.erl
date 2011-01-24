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
-behaviour(gen_server).
-export([att_foldl/3]).

% public API
-export([open/1, write/2, sync/1, close/1]).

% gen_server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_cast/2, handle_call/3, handle_info/2]).

-include("couch_db.hrl").
-record(ext_stream, {path, fd=nil, len=0, md5}).
-define(BUFFER_SIZE, 16384).

%%% Interface functions %%%

open(Path) ->
    gen_server:start_link(?MODULE, Path, []).

write(Stream, Bin) when is_binary(Bin) ->
    gen_server:call(Stream, {write, Bin}, infinity).

sync(Stream) ->
    gen_server:call(Stream, sync, infinity).

close(Stream) ->
    gen_server:call(Stream, close, infinity).

att_foldl(#att{location={external, _}=Location,md5=Md5}, Fun, Acc) ->
    Path = couch_db:get_external_path(Location),
    {ok, Fd} = file:open(Path, [read, raw, binary]),
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

%% gen_server callbacks.

init(Path) ->
    case file:open(Path, [raw, binary, write, append, exclusive]) of
        {ok, Fd} ->
            {ok, #ext_stream{
                path=Path,
                fd=Fd,
                md5=couch_util:md5_init()
                }
            };
        Error ->
            {stop, Error}
    end.

terminate(_Reason, #ext_stream{fd=nil}) ->
    ok;
terminate(_Reason, #ext_stream{fd=Fd}) ->
    file:close(Fd).

handle_call(sync, _From, #ext_stream{fd=Fd}=Stream) ->
    {reply, file:datasync(Fd), Stream};
handle_call({write, Bin}, _From, #ext_stream{len=Len, md5=Md5, fd=Fd}=Stream) ->
    case file:write(Fd, Bin) of
        ok ->
            {reply, ok, Stream#ext_stream{
                len=Len + iolist_size(Bin),
                md5=couch_util:md5_update(Md5, Bin)
                }};
        Error ->
            {reply, Error, Stream}
    end;
handle_call(close, _From, #ext_stream{len=Len,md5=Md5,fd=Fd}=Stream) ->
    ok = file:close(Fd),
    FinalMd5 = couch_util:md5_final(Md5),
    {stop, normal, {{0, Len}, Len, FinalMd5}, Stream#ext_stream{fd=nil}}.

handle_cast(_Msg, State) ->
    {noreply,State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
