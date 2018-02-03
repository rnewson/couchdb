-module(chttpd2_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    Port = config:get_integer("chttpd", "port", 5984),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", chttpd2_welcome, []},
            {"/:account/:dbname", chttpd2_db, []},

            {"/:account/:dbname/_changes", chttpd2_changes, []},
            {"/:account/:dbname/_all_docs", chttpd2_all_docs, []},

            {"/:account/:dbname/_design/:docid", chttpd2_doc, <<"_design/">>},
            {"/:account/:dbname/_local/:docid", chttpd2_doc, <<"_local/">>},
            {"/:account/:dbname/:docid", chttpd2_doc, <<"">>},
            {"/:account/:dbname/:docid/:attname", chttpd2_att, []}

        ]}
    ]),
    cowboy:start_clear(
        cowboy_clear,
        [{port, Port + 1}],
        #{env => #{dispatch => Dispatch}
    }).

stop(_State) ->
    ok.
