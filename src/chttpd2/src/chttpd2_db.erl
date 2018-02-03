-module(chttpd2_db).

% handler
-export([init/2]).

% REST
-export([
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    delete_completed/2,
    delete_resource/2,
    resource_exists/2
]).

-export([
    create_db/2,
    get_db_info/2
]).


init(Req, _State) ->
    {cowboy_rest, Req, #{db_info => nil, delete_completed => nil}}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.


content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, get_db_info}], Req, State}.


resource_exists(Req, State) ->
    try
        {ok, DbInfo} = fabric:get_db_info(chttpd2_util:dbname(Req)),
        {true, Req, State#{db_info := DbInfo}}
    catch
        _:_ ->
            {false, Req, State}
    end.


content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, create_db}], Req, State}.


delete_resource(Req, State) ->
    case fabric:delete_db(chttpd2_util:dbname(Req), []) of
        ok ->
            {true, Req, State#{delete_completed := true}};
        accepted ->
            {true, Req, State#{delete_completed := false}};
        _ ->
            {false, Req, State}
    end.


delete_completed(Req, #{delete_completed := DeleteCompleted} = State) ->
    {DeleteCompleted, Req, State}.


create_db(Req, State) ->
    #{q := Q, n := N} = cowboy_req:match_qs([
        {n, int, config:get_integer("cluster", "n", 3)},
        {q, int, config:get_integer("cluster", "q", 8)}]
    , Req),
    case fabric:create_db(chttpd2_util:dbname(Req), [{q, Q}, {n, N}]) of
        ok ->
            {true, Req, State};
        accepted ->
            {true, Req, State};
        _ ->
            {false, Req, State}
    end.


get_db_info(Req, #{db_info := DbInfo} = State) ->
    Body = jiffy:encode({DbInfo}),
    {Body, Req, State}.
