#!/usr/bin/env escript
%% -*- erlang -*-

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

main(_) ->
    test_util:init_code_path(),
    etap:plan(12),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    Choose = fun(A, _) -> A end,

    One = {1, {"1","foo",[]}},

    etap:is(
        {[One], no_conflicts},
        couch_key_tree:merge([], One, Choose, 10),
        "The empty tree is the identity for merge."
    ),
    
    etap:is(
        {[One], no_conflicts},
        couch_key_tree:merge([One], One, Choose, 10),
        "Merging is reflexive."
    ),

    TwoSibs = [{1, {"1","foo",[]}},
               {1, {"2","foo",[]}}],

    etap:is(
        {TwoSibs, no_conflicts},
        couch_key_tree:merge(TwoSibs, One, Choose, 10),
        "Merging a prefix of a tree with the tree yields the tree."
    ),

    etap:is(
        {ThreeSibs, conflicts},
        couch_key_tree:merge(TwoSibs, Three, Choose, 10),
        "Merging a third unrelated branch leads to a conflict."
    ),

    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge([TwoChild], TwoChild, Choose, 10),
        "Merging two children is still reflexive."
    ),

    etap:is(
        {[TwoChildSibs], no_conflicts},
        couch_key_tree:merge([TwoChildSibs], TwoChildSibs, Choose, 10),
        "Merging a tree to itself is itself."),

    TwoChildPlusSibs =
        {1, {"1","foo", [{"1a", "bar", [{"1aa", "bar", []}]},
                         {"1b", "bar", []}]}},

    etap:is(
        {[TwoChildPlusSibs], no_conflicts},
        couch_key_tree:merge([TwoChild], TwoChildSibs, Choose, 10),
        "Merging tree of uneven length at node 2."),

    Stemmed1b = {2, {"1a", "bar", []}},
    
    etap:is(
        {[TwoChildSibs], no_conflicts},
        couch_key_tree:merge([TwoChildSibs], Stemmed1b, Choose, 10),
        "Merging a tree with a stem."
    ),

    etap:is(
        {[TwoChildSibs2], no_conflicts},
        couch_key_tree:merge([TwoChildSibs2], Stemmed1bb, Choose, 10),
        "Merging a stem at a deeper level."
    ),

    StemmedTwoChildSibs2 = [{2,{"1a", "bar", []}},
                            {2,{"1b", "bar", [{"1bb", "boo", []}]}}],

    etap:is(
        {StemmedTwoChildSibs2, no_conflicts},
        couch_key_tree:merge(StemmedTwoChildSibs2, Stemmed1bb, Choose, 10),
        "Merging a stem at a deeper level against paths at deeper levels."
    ),

    Stemmed1aa = {3, {"1aa", "bar", []}},
    
    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge([TwoChild], Stemmed1aa, Choose, 10),
        "Merging a single tree with a deeper stem."
    ),

    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge([TwoChild], Stemmed1a, Choose, 10),
        "Merging a larger stem."
    ),

    etap:is(
        {[Stemmed1a], no_conflicts},
        couch_key_tree:merge([Stemmed1a], Stemmed1aa, Choose, 10),
        "More merging."
    ),

    Expect1 = [OneChild, Stemmed1aa],
    etap:is(
        {Expect1, conflicts},
        couch_key_tree:merge([OneChild], Stemmed1aa, Choose, 10),
        "Merging should create conflicts."
    ),

    etap:is(
        {[TwoChild], no_conflicts},
        couch_key_tree:merge(Expect1, TwoChild, Choose, 10),
        "Merge should have no conflicts."
    ),

    %% this test is based on couch-902-test-case2.py
    %% foo has conflicts from replication at depth two
    %% foo3 is the current value
    Foo = {1, {"foo",
               "val1",
               [{"foo2","val2",[]},
                {"foo3", "val3", []}
               ]}},
    %% foo now has an attachment added, which leads to foo4 and val4
    %% off foo3
    Bar = {1, {"foo",
               [],
               [{"foo3",
                 [],
                 [{"foo4","val4",[]}
                  ]}]}},
    %% this is what the merge returns
    %% note that it ignore the conflicting branch as there's no match
    FooBar = {1, {"foo",
               "val1",
               [{"foo2","val2",[]},
                {"foo3", "val3", [{"foo4","val4",[]}]}
               ]}},

    etap:is(
      {[FooBar], no_conflicts},
      couch_key_tree:merge([Foo],Bar, Choose, 10),
      "Merging trees with conflicts ought to behave."
    ),

    ok.
