-module(erlsom_gexf_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlsom/src/erlsom.hrl").
-include_lib("erlsom/src/erlsom_parse.hrl").

compile_gexf_xsd() ->
    XsdDir = filename:join(code:priv_dir(erlsom), "gexf/schema/"),
    XsdFilePath = filename:join(XsdDir, "gexf.xsd"),
    erlsom:compile_xsd_file(XsdFilePath, [{include_dirs, [XsdDir]}]).

parse_gexf_file(DataFileName, XsdModel) ->
    DataPath = code:priv_dir(erlsom) ++ "/gexf/data/" ++ DataFileName,
    erlsom:parse_file(DataPath, XsdModel).

compile_schema_test() ->
    {ok, _Model} = compile_gexf_xsd().

unique_namespaces_test() ->
    {ok, Model} = compile_gexf_xsd(),
    Namespaces = Model#model.nss,
    ?assertEqual(lists:usort(Namespaces), Namespaces),
    ok.

parse_file_test() ->
    {ok, Model} = compile_gexf_xsd(),
    {ok, _Tree} = parse_gexf_file("test.gexf", Model),
    ok.


%% @doc makeAttrRef returns ":parent-content". It is an error.
leading_ns_delimeter_test_() ->
    NS =  [#ns{uri = "http://www.gexf.net/1.2draft", prefix = ""}],
    Ref = #qname{uri = "http://www.gexf.net/1.2draft",
                 localPart = "parent-content",
                 prefix = "ns1",
                 mappedPrefix = []},
    [?_assertEqual("parent-content", erlsom_lib:makeAttrRef(Ref, NS))].

