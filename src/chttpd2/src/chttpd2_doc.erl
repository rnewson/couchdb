-module(chttpd2_doc).

-include_lib("couch/include/couch_db.hrl").

% handler
-export([init/2]).

% REST
-export([
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    generate_etag/2,
    resource_exists/2
]).

-export([
    get_doc/2,
    update_doc/2
]).


init(Req, DocIdPrefix) ->
    {cowboy_rest, Req, #{doc_id_prefix => DocIdPrefix, doc => nil}}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.


resource_exists(Req, #{doc_id_prefix := DocIdPrefix} = State) ->
    DocId = <<DocIdPrefix/binary, (cowboy_req:binding(docid, Req))/binary>>,
    case fabric:open_doc(chttpd2_util:dbname(Req), DocId, []) of
        {ok, Doc} ->
            {true, Req, State#{doc := Doc}};
        {not_found, _} ->
            {false, Req, State}
    end.


generate_etag(Req, #{doc := #doc{revs = {Start, [Rev | _]}}} = State) ->
    ETag = couch_doc:rev_to_str({Start, Rev}),
    {{strong, ETag}, Req, State}.


content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, get_doc}], Req, State}.


content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, update_doc}], Req, State}.


get_doc(Req, #{doc := Doc} = State) ->
    Body = couch_doc:to_json_obj(Doc, []),
    {jiffy:encode(Body), Req, State}.


update_doc(Req, State) ->
    %DocId  = cowboy_req:binding(docid, Req),
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    Doc = couch_doc:from_json_obj_validate(Body),
    try
        case fabric:update_doc(chttpd2_util:dbname(Req), Doc, []) of
            {ok, _} ->
                {true, Req1, State};
            {accepted, _} ->
                {true, Req1, State}
        end
    catch
        conflict ->
            {false, Req1, State}
    end.
