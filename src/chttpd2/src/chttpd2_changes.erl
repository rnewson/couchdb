-module(chttpd2_changes).
-include_lib("couch/include/couch_db.hrl").

% handler
-export([init/2]).

init(Req, State) ->
    init(cowboy_req:method(Req), Req, State).


init(<<"GET">>, Req, State) ->
    DbName = chttpd2_util:dbname(Req),
    #{since := Since, limit := Limit} = cowboy_req:match_qs([
        {since, [], 0},
        {limit, int, 1000000000000000}
    ], Req),
    ChangesArgs = #changes_args{
        since = Since,
        limit = Limit
    },
    {ok, Req1} = fabric:changes(DbName, fun changes_callback/2, Req, ChangesArgs),
    {ok, Req1, State};

init(_Method, Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"allow">>, <<"GET">>, Req),
    Req2 = cowboy_req:reply(405, Req1),
    {ok, Req2, State}.


changes_callback(start, Req) ->
    Req1 = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"application/json">>}, Req),
    Data = <<"{\"results\":[\n">>,
    ok = cowboy_req:stream_body(Data, nofin, Req1),
    {ok, {<<"">>, Req1}};

changes_callback({change, Change}, {Prefix, Req}) ->
    Data = jiffy:encode(Change),
    ok = cowboy_req:stream_body([Prefix, Data, $\n], nofin, Req),
    {ok, {<<",">>, Req}};

changes_callback({stop, EndSeq, Pending}, {_Prefix, Req}) ->
    Data = [
        <<"\n],\n\"last_seq\":">>,
        jiffy:encode(EndSeq),
        <<",\"pending\":">>,
        jiffy:encode(Pending),
        <<"}">>
    ],
    ok = cowboy_req:stream_body(Data, fin, Req),
    {ok, Req}.
