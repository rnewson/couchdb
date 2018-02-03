-module(chttpd2_util).

-export([dbname/1]).

dbname(Req) ->
    Account = cowboy_req:binding(account, Req),
    DbName = cowboy_req:binding(dbname, Req),
    <<Account/binary, $/, DbName/binary>>.
