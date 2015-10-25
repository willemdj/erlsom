-module(erlsom_tests).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

gexf_test_() ->
    {"Test XML/XSD in GEXF format.", {module, erlsom_gexf_tests}}.

all_test_() ->
    {"Test XSD with the xs:all tag.",
     verify_stability_(["all", "all.xsd"],
                       ["all", "all.xml"],
                       [])}.

extension_test_() ->
    {"Test XSD type extensions.",
     verify_stability_(["extension", "extension.xsd"],
                       ["extension", "extension.xml"],
                       [])}.

%% @doc
%% compile the XSD schema file with the given relative path, example:
%%   compile_xsd(["all", "all.xsd"], [])
compile_xsd(Path, IncludePaths) ->
    IncludeDirs = [priv_path(I) || I <- IncludePaths],
    erlsom:compile_xsd_file(priv_path(Path), [{include_dirs, IncludeDirs}]).

%% @doc
%% parse an xml document with a compiled XSD model, example:
%%   parse_file(["all", "all.xml"], Model)
parse_file(Path, Model) ->
    erlsom:parse_file(priv_path(Path), Model).

%% @doc
%% verify the parser/generator stability, example:
%%   verify_stability(["all", "all.xsd"], ["all", "all.xml"], [])
verify_stability(XsdPath, XmlPath, IncludeDirs) ->
    {ok, Model} = compile_xsd(XsdPath, IncludeDirs),
    {ok, Tree1} = parse_file(XmlPath, Model),
    {ok, XML}   = erlsom:write(Tree1, Model),
    {ok, Tree2} = erlsom:parse(XML, Model),
    {lists:last(XmlPath), ?_assertEqual(Tree1, Tree2)}.

%% @doc
%% test generator function for verify_stability tests
verify_stability_(XsdPath, XmlPath, IncludeDirs) ->
    fun() -> verify_stability(XsdPath, XmlPath, IncludeDirs) end.

priv_path(Path) ->
    filename:join([code:priv_dir(erlsom) | Path]).
