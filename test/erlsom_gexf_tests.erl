-module(erlsom_gexf_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlsom/src/erlsom.hrl").

compile_schema_test() ->
    DataDir = filename:join(code:priv_dir(erlsom), "gexf"),
    XsdFilePath = filename:join(DataDir, "gexf.xsd"),
    {ok, _Model} = erlsom:compile_xsd_file(XsdFilePath, [{include_dirs, [DataDir]}]).


%% @doc makeAttrRef returns ":parent-content". It is an error.
leading_ns_delimeter_test_() ->
    NS =  [#ns{uri = "http://www.gexf.net/1.2draft", prefix = ""}],
    Ref = #qname{uri = "http://www.gexf.net/1.2draft",
                 localPart = "parent-content",
                 prefix = "ns1",
                 mappedPrefix = []},
    [?_assertEqual("parent-content", erlsom_lib:makeAttrRef(Ref, NS))].

