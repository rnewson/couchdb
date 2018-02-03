-module(chttpd2_all_docs).
-include_lib("couch/include/couch_db.hrl").

% handler
-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            DbName = chttpd2_util:dbname(Req),
            Options = [],
            {ok, Req1} = fabric:all_docs(DbName, Options, fun all_docs_callback/2, Req, []),
            {ok, Req1, State};
        _ ->
            Req1 = cowboy_req:set_resp_header(<<"allow">>, <<"GET">>, Req),
            Req2 = cowboy_req:reply(405, Req1),
            {ok, Req2, State}
    end.

all_docs_callback({meta, _Meta}, Req) ->
    Req1 = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"application/json">>}, Req),
    Data = [<<"{\"rows\":[\n">>],
    ok = cowboy_req:stream_body(Data, nofin, Req1),
    {ok, {<<"">>, Req1}};

all_docs_callback({row, Row}, {Prefix, Req}) ->
    Obj = {[
        {id, couch_util:get_value(id, Row)},
        {key, couch_util:get_value(key, Row)},
        {value, couch_util:get_value(value, Row)},
        {doc, couch_util:get_value(doc, Row, null)}
    ]},
    Data = jiffy:encode(Obj),
    ok = cowboy_req:stream_body([Prefix, Data, $\n], nofin, Req),
    {ok, {<<",">>, Req}};

all_docs_callback(complete, {_Prefix, Req}) ->
    Data = <<"]}">>,
    ok = cowboy_req:stream_body(Data, fin, Req),
    {ok, Req}.
