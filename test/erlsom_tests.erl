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


%%
%%  Check if xsi:type is parsed and written correctly.
%%
%%  This test was introduced to check/fix the following bugs:
%%    * The ext namespace was not added to the corresponding element when writing model to the XSD.
%%    * The xsi namespace was duplicated with different prefix if the model had xsi namespace defined with other prefix.
%%
%%  Before the fix, the `Written' XML was looking like this:
%%  ```
%%      <b:data
%%          xmlns:b="urn:erlsom/xsi_type/base"
%%          xmlns:pre1="http://www.w3.org/2001/XMLSchema-instance"
%%          pre1:type="ext:ExtType"
%%          xsi:type="e:ExtType"
%%          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
%%          <b:base_info>a</b:base_info>
%%          <e:ext_info xmlns:e="urn:erlsom/xsi_type/ext">b</e:ext_info>
%%      </b:data>
%%  '''
xsi_type_write_test() ->
    %
    % Parse the XSD model.
    {ok, Base} = erlsom:compile_xsd_file(
        priv_path(["xsi_type", "base.xsd"]),
        [
            {include_any_attribs, true},
            {prefix, "b"}
        ]
    ),
    {ok, Ext} = erlsom:compile_xsd_file(
        priv_path(["xsi_type", "ext.xsd"]),
        [
            {include_any_attribs, true},
            {prefix, "e"},
            {include_dirs, [priv_path(["xsi_type"])]},
            {include_files, [{"urn:erlsom/xsi_type/base", "b", priv_path(["xsi_type", "base.xsd"])}]}
        ]
    ),
    Model = erlsom:add_model(Base, Ext),
    io:format("Model=~p~n", [Model]),
    %
    % Parse the XML.
    {ok, Xml} = file:read_file(priv_path(["xsi_type", "ext.xml"])),
    {ok, Parsed1} = erlsom:parse(Xml,     Model), io:format("Parsed1=~p~n", [Parsed1]),
    {ok, Written} = erlsom:write(Parsed1, Model), io:format("Written=~p~n", [Written]),
    {ok, Parsed2} = erlsom:parse(Written, Model), io:format("Parsed2=~p~n", [Parsed2]),
    ?assertEqual(
        erlang:setelement(2, Parsed1, []),  % Compare ignoring the extra attributes, because they
        erlang:setelement(2, Parsed2, [])   % have type names with prefixes, as defined in the XML.
    ).

% TODO: XSI:type and xsi:nil in one element.


%%
%%  Check, if document can be parsed in the case, when an element is put to the
%%  global namespace explicitly (`xmlns=""') and has derived type specified.
%%
xsi_type_no_prefix_read_test() ->
    %
    % Parse the XSD model.
    {ok, Model} = erlsom:compile_xsd_file(
        priv_path(["xsi_type_no_prefix", "test.xsd"]),
        [
            {include_any_attribs, true},
            {prefix, "t"}
        ]
    ),
    io:format("Model=~p~n", [Model]),
    %
    % Parse the XML.
    {ok, Xml} = file:read_file(priv_path(["xsi_type_no_prefix", "test.xml"])),
    {ok, Parsed} = erlsom:parse(Xml, Model),
    io:format("Parsed=~p~n", [Parsed]),
    ?assertMatch({'ExtType', _, "base", "ext"}, Parsed).


