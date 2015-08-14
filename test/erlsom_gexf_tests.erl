-module(erlsom_gexf_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlsom/src/erlsom.hrl").
-include_lib("erlsom/src/erlsom_parse.hrl").

-define(XSD_FILE, ["gexf", "schema", "gexf.xsd"]).
-define(INCLUDE_PATHS, [["gexf", "schema"]]).

compile_schema_test() ->
    {ok, _Model} = erlsom_tests:compile_xsd(?XSD_FILE, ?INCLUDE_PATHS).

unique_namespaces_test() ->
    {ok, Model} = erlsom_tests:compile_xsd(?XSD_FILE, ?INCLUDE_PATHS),
    Namespaces = Model#model.nss,
    ?assertEqual(lists:usort(Namespaces), Namespaces),
    ok.

parse_file_test() ->
    {ok, Model} = erlsom_tests:compile_xsd(?XSD_FILE, ?INCLUDE_PATHS),
    {ok, _Tree} = erlsom_tests:parse_file(["gexf", "data", "test.gexf"], Model),
    ok.


%% @doc makeAttrRef returns ":parent-content". It is an error.
leading_ns_delimeter_test_() ->
    NS =  [#ns{uri = "http://www.gexf.net/1.2draft", prefix = ""}],
    Ref = #qname{uri = "http://www.gexf.net/1.2draft",
                 localPart = "parent-content",
                 prefix = "ns1",
                 mappedPrefix = []},
    [?_assertEqual("parent-content", erlsom_lib:makeAttrRef(Ref, NS))].


stability_test_() ->
    [{T,
      erlsom_tests:verify_stability_(
        ?XSD_FILE, ["gexf", "data", T], ?INCLUDE_PATHS)}
     || T <- ["test.gexf", "basic.gexf", "data.gexf", "dynamics.gexf"]].
