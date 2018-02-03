-module(chttpd2_welcome).

-include_lib("couch/include/couch_db.hrl").

% handler
-export([init/2]).

% REST
-export([
    content_types_provided/2,
    to_json/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.


content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.


to_json(Req, State) ->
    Body = jiffy:encode({[
        {couchdb, <<"Welcome">>},
        {version, ?l2b(couch_server:get_version())},
        {vendor, {[
            {name, <<"IBM Cloudant">>},
            {version, ?l2b(cloudant_util:get_version())},
            {variant, ?l2b(cloudant_util:build_type())}
        ]}},
        {features, get_features()}
    ]}),
    {Body, Req, State}.


get_features() ->
    Features = [
        check_geo_enabled(),
        check_replicator_scheduler(),
        check_iam()
    ],
    [Feature || {Feature, true} <- Features].


check_geo_enabled() ->
    {<<"geo">>, config:get_boolean("hastings", "enabled", false)}.


check_replicator_scheduler() ->
    {<<"scheduler">>, code:which(couch_replicator_scheduler) =/= non_existing}.


check_iam() ->
    {<<"iam">>, code:which(epep) =/= non_existing}.
