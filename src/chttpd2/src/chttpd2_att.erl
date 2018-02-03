-module(chttpd2_att).
-include_lib("couch/include/couch_db.hrl").

% handler
-export([init/2]).

init(Req, State) ->
    init(cowboy_req:method(Req), Req, State).

init(<<"GET">>, Req, State) ->
    DocId = cowboy_req:binding(docid, Req),
    AttName = cowboy_req:binding(attname, Req),

    case fabric:open_doc(chttpd2_util:dbname(Req), DocId, []) of
        {ok, Doc} ->
            [Att] = [A || A <- Doc#doc.atts, couch_att:fetch(name, A) == AttName],
            {Fd, _} = couch_att:fetch(data, Att),
            MonitorRef = monitor(process, Fd),
            Req1 = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"application/octet-stream">>}, Req),
            couch_att:foldl_decode(Att, fun(Data, _) ->
                ok = cowboy_req:stream_body(Data, nofin, Req1) end, {ok, Req1}),
            ok = cowboy_req:stream_body(<<>>, fin, Req1),
            demonitor(MonitorRef, [flush]),
            {ok, Req1, State};
        {not_found, _} ->
            Req1 = cowboy_req:reply(404, Req),
            {ok, Req1, State}
    end;

init(_Method, Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"allow">>, <<"GET">>, Req),
    Req2 = cowboy_req:reply(405, Req1),
    {ok, Req2, State}.
