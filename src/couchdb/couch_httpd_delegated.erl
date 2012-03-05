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

-module(couch_httpd_delegated).
-include("couch_db.hrl").
-export([handle_delegated_auth_req/1, delegated_authentication_handler/1]).

-import(couch_httpd, [send_json/3]).

handle_delegated_auth_req(Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    couch_httpd:validate_ctype(Req, "application/json"),
    {Props} = couch_httpd:json_body(Req),
    Name = validate_name(couch_util:get_value(<<"name">>, Props)),
    Roles = validate_roles(couch_util:get_value(<<"roles">>, Props, [])),
    Cookie = make_cookie(Name, Roles),
    send_json(Req, 200, {[{ok, true}, {cookie, Cookie}]}).

delegated_authentication_handler(#httpd{mochi_req=MochiReq}=Req) ->
    case MochiReq:get_cookie_value("DelegatedAuth") of
    undefined ->
        Req;
    [] ->
        Req;
    Cookie ->
        [User, Roles, TimeStr | MacParts] = try
            DelegatedAuth = couch_util:decodeBase64Url(Cookie),
            string:tokens(?b2l(DelegatedAuth), ":")
        catch
            _:_Error ->
                throw({bad_request, <<"Malformed DelegatedAuth cookie.">>})
        end,
        CurrentTime = make_cookie_time(),
        Secret = ensure_delegated_auth_secret(),
        ExpectedMac = crypto:sha_mac(Secret, User ++ ":" ++ Roles ++ ":" ++ TimeStr),
        ActualMac = ?l2b(string:join(MacParts, ":")),
        Timeout = timeout(),
        case (catch erlang:list_to_integer(TimeStr, 16)) of
            TimeStamp when CurrentTime < TimeStamp + Timeout ->
                case couch_util:verify(ExpectedMac, ActualMac) of
                    true ->
                        Req#httpd{user_ctx=#user_ctx{
                            name=?l2b(User),
                            roles=[?l2b(Role) || Role <- string:tokens(Roles, ",")]}};
                    _Else ->
                        Req
                end;
            _Else ->
                Req
        end
    end.

validate_name(Name) when is_binary(Name) ->
    ?b2l(Name);
validate_name(_Name) ->
    throw({bad_request, <<"Malformed or missing 'name'">>}).

validate_roles(Roles) ->
    case validate_roles(Roles, []) of
        [] -> ",";
        Else -> Else
    end.

validate_roles([], Acc) ->
    string:join(Acc, ",");
validate_roles([Role|Rest], Acc) when is_binary(Role) ->
    validate_roles(Rest, [?b2l(Role)|Acc]);
validate_roles(_, _Acc) ->
    throw({bad_request, <<"Malformed roles">>}).

make_cookie(Name, Roles) ->
    TimeStamp = make_cookie_time(),
    Secret = ensure_delegated_auth_secret(),
    SessionData = Name ++ ":" ++ Roles ++ ":" ++ erlang:integer_to_list(TimeStamp, 16),
    Mac = crypto:sha_mac(Secret, SessionData),
    {"Set-Cookie", CookieValue} = mochiweb_cookies:cookie("DelegatedAuth",
        couch_util:encodeBase64Url(SessionData ++ ":" ++ ?b2l(Mac)),
        [{path, "/"}, {max_age, timeout()}]),
    ?l2b(CookieValue).

make_cookie_time() ->
    {NowMS, NowS, _} = erlang:now(),
    NowMS * 1000000 + NowS.

ensure_delegated_auth_secret() ->
    case couch_config:get("delegated_auth", "secret", nil) of
        nil ->
            NewSecret = ?b2l(couch_uuids:random()),
            couch_config:set("delegated_auth", "secret", NewSecret),
            NewSecret;
        Secret -> Secret
    end.

%% timeout defaults to 1 hour
timeout() ->
    list_to_integer(couch_config:get("delegated_auth", "timeout", "3600")).
