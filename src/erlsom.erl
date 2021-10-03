%%% Copyright (C) 2006 - 2008 Willem de Jong
%%%
%%% This file is part of Erlsom.
%%%
%%% Erlsom is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as
%%% published by the Free Software Foundation, either version 3 of
%%% the License, or (at your option) any later version.
%%%
%%% Erlsom is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with Erlsom.  If not, see
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: w.a.de.jong@gmail.com

%%% ====================================================================
%%% Support for XML Schema in Erlang
%%% ====================================================================

%%% This is the user interface for the Erlsom functions.

-module(erlsom).

%% user interface
-export([compile/1, compile/2, compile/3,
         compile_file/1, compile_file/2, compile_file/3,
         compile_xsd/1, compile_xsd/2, compile_xsd_file/1, compile_xsd_file/2,
         parse/2, parse_file/2,
         simple_form/1, simple_form/2,
         simple_form_file/1, simple_form_file/2,
         scan/2, scan/3, scan_file/2, scan_file/3,
         write/2, write/3,
         parse_sax/3, parse_sax/4,
         sax/3, sax/4,
         write_hrl/2, write_hrl/3,
         write_xsd_hrl_file/2, write_xsd_hrl_file/3,
         write_hrl_file/2, write_hrl_file/3, write_hrl_file/4,
         add_xsd_file/3,
         add_file/3, add_xsd_model/1, add_model/2]).

-include("erlsom.hrl").
-include("exception.hrl").
-include("erlsom_parse.hrl").

-type characters() :: string() | binary().
-type prefix() :: string().
-type uri() :: string().
-type local_name() :: string().
-type attribute() :: erlsom_sax:attribute().

-type model() :: #model{}.
-export_type([model/0]).
-type sax_event() :: startDocument | endDocument |
  {startPrefixMapping, prefix(), uri()} | {endPrefixMapping, prefix()} |
  {startElement, uri(), local_name(), prefix(), [attribute()]} |
  {endElement, uri(), local_name(), prefix()} |
  {characters, characters()} |
  {ignorableWhitespace, characters()} |
  {error, any()} |
  {internalError, any()}.
-export_type([sax_event/0]).

%%----------------------------------------------------------------------
%% Function: compile_xsd/2
%% Purpose: compile an XSD into a structure to be used by erlsom:parse().
%% Args:
%%     XSD  = string(): the XSD.
%%     Options = [Option]
%%     Option = {prefix, Prefix} |
%%              {strict, boolean()} |
%%              {include_fun, Include_fun} |
%%              {include_dirs, Include_dirs} |
%%              {include_files, Include_list}
%%
%%     'Prefix' is prefixed to the record names in the XSD. It should be
%%        be a string or 'undefined'. If it is 'undefined', no prefix
%%        will be applied. The default is 'undefined'.
%%
%%     'Include_fun' is a function that finds the files that are
%%        included or imported in the XSD. It should be a function that
%%        takes 4 arguments:
%%           Namespace (from the XSD). This is a string or 'undefined'
%%           SchemaLocation (from the XSD). This is a string or 'undefined'
%%           Include_files. This is the value of the ‘include_files’ option if this
%%             option was passed to compile_xsd(); [] otherwise.
%%           Include_Dirs. This is the value of the Include_dirs option if provided,
%%             'undefined' otherwise.
%%           Prefix_list
%%        Include_fun should return the {XSD, Prefix}, where XSD is a
%%           XSD = string()
%%           Prefix = string or 'undefined', see above.
%%        Include_fun defaults to a function that searches the directories
%%        in Include_dirs for a file with the name SchemaLocation; it returns
%%        'undefined' for the prefix, unless this was specified in the
%%        prefix_list.
%%
%%     'Include_dirs' is a list of directories (strings), separated by commas.
%%       It defaults to ["."].
%%
%%     'Include_files' is a list of tuples {Namespace, Prefix, Location}.
%%
%%      'strict' - this enforces type checking of a number of additional
%%         types. If strict is false, the following types are checked and
%%         converted:
%%         - integer - converted to and from erlang integer
%%         - boolean - converted to an from an erlang boolean
%%         - qname   - converted to and from a #qname{} record
%%         All other types are treated as strings.
%%
%%         If strict is true, additionally the
%%         following types are checked and converted:
%%         - positiveInteger, ..TODO - all translated to integer
%%         - float - translated to/from a float or the atoms 'NaN',
%%           ..TODO
%%
%% Behaviour for includes:
%%
%% If 'include_fun' option is present, this function will be called. This should
%% return both the prefix and the file.
%%
%% Otherwise, if the 'includes' option is present, the list provided with this
%% option will be searched for a matching namespace. If this is found, the
%% specified prefix will be used. If a file is also specified, then this file will
%% be used. If no file is specified (value is undefined), then the 'location'
%% attribute and the 'include_dirs' option will be used to locate the file.
%%
%% If the 'includes' option is not present, or if the namespace is not found, then
%% the file will be searched for in the include_dirs (based on the 'location'
%% attribute). No prefix will be used.
%%
%% Returns: {ok, Model}, where Model is the internal structure, see
%% xml2struct.erl
%%----------------------------------------------------------------------
compile_xsd(Xsd) ->
  compile_xsd(Xsd, []).

compile_xsd(Xsd, Options) ->
  case catch erlsom_compile:compile(Xsd, Options) of
    {error, Message} -> {error, Message};
    {'EXIT', Message} -> throw({'EXIT', Message});
    Result -> Result
  end.

compile_xsd_file(Xsd) ->
  compile_xsd_file(Xsd, []).

compile_xsd_file(XsdFile, Options) ->
  case file:read_file(XsdFile) of
    {ok, Bin} ->
      compile_xsd(Bin, Options);
    Error ->
      Error
  end.

%%----------------------------------------------------------------------
%% Note: these functions should no longer be used, use compile_xsd!
%%
%% Function: compile/3
%% Purpose: compile an XSD into a structure to be used by erlsom:parse().
%% Args:
%%     XSD  = string(): the XSD.
%%     Prefix = string
%%     Namespaces = [#ns]
%%
%%     'Prefix' is prefixed to the record names in the
%%     in the XSD.
%%     'Namespaces' should include the URIs of imported namespaces;
%%     the purpose is to define the prefix for those.
%%
%% Returns: {ok, Model}, where Model is the internal structure, see
%% xml2struct.erl
%%----------------------------------------------------------------------
compile(Xsd) ->
  compile_xsd(Xsd, [{prefix, "p"}]).

compile(Xsd, Prefix) ->
  compile_xsd(Xsd, [{prefix, Prefix}]).

compile(Xsd, Prefix, Namespaces) ->
  compile_xsd(Xsd, [{prefix, Prefix}, {namespaces, Namespaces}]).

compile_file(XsdFile) ->
  compile_file(XsdFile, "p").

compile_file(XsdFile, Prefix) ->
  compile_file(XsdFile, Prefix, []).

compile_file(XsdFile, Prefix, Namespaces) ->
  case file:read_file(XsdFile) of
    {ok, Bin} ->
      compile(Bin, Prefix, Namespaces);
    Error ->
      Error
  end.

%%----------------------------------------------------------------------
%% Function: scan/2
%% Purpose: translate an XML document that conforms to the XSD to a
%%     structure of records. (The XSD is represented by the 'Model', the
%%     result of the translation of the XSD by erlsom:compile().)
%% Args:
%%     Xml = string(): the Xml document.
%%     Model = the internal representation of the XSD; the result of
%%     erlsom:compile().
%%     Options = [Option].
%%     Option = {continuation_function, Continuation_function,
%%               Continuation_state}
%%
%% If specified, the continuation function is called whenever the end of
%% the input XML document is reached before the parsing of the XML has finished.
%% The function should have 1 argument (Continuation_state). It should return
%% a tuple {NewData, NewState}, where NewData should be the next block of
%% data (a list of unicode code points), and NewState is the information that
%% is passed to the next invocation.
%%
%% Returns: {ok, Structure, TrailingCharacters}, where
%%     Structure is a structure of records that represent the XML
%%     document. See the documentation for the mapping;
%%     TrailingCharacters = any characters in the input string after the
%%       XML document.
%%----------------------------------------------------------------------
scan(Xml, Model) ->
  scan(Xml, Model, []).

scan(Xml, #model{value_fun = ValFun} = Model, Options) ->
  State = #state{model=Model, namespaces=[], value_fun = ValFun},
  case lists:keysearch(acc, 1, Options) of
    {value, {_, Acc}} ->
      scan2(Xml, State#state{value_acc = Acc},
            lists:keydelete(acc, 1, Options));
    false ->
      scan2(Xml, State, Options)
  end.

%%cFunctionWrapper(T, S = #state{continuationState = {F, CS}}) ->
  %%{Data, CS2} = F(CS),
  %%{T ++ Data, S#state{continuationState = {F, CS2}}}.

scan2(Xml, State, Options) ->
  case catch erlsom_sax:parseDocument(Xml, State,
                                      fun erlsom_parse:xml2StructCallback/2,
                                      Options) of
    {error, Message} -> {error, Message};
    {'EXIT', Message} -> throw({'EXIT', Message});
    {ok, Structure, Tail} -> {ok, Structure, Tail}
  end.


scan_file(File, Model) ->
  scan_file(File, Model, []).

scan_file(File, Model, Options) ->
  case file:read_file(File) of
    {ok, Bin} ->
      try
        scan(Bin, Model, Options)
      catch
        throw:Reason -> {error, Reason};
        exit:Reason -> throw({'EXIT',Reason});
        ?EXCEPTION(error, Reason, Stacktrace) -> throw({'EXIT',{Reason, ?GET_STACK(Stacktrace)}})
      end;
    Error ->
      Error
  end.

%%----------------------------------------------------------------------
%% Function: parse/2
%% Deprecated. Same as 'scan/2', but without the trailing
%% characters. If there are any trailing characters they are ignored.
%%----------------------------------------------------------------------
parse(Xml, Model) ->
  case scan(Xml, Model) of
    {error, Message} -> {error, Message};
    {ok, Structure, _Tail} -> {ok, Structure}
  end.

parse_file(File, Model) ->
  case file:read_file(File) of
    {ok, Bin} ->
      parse(erlsom_lib:toUnicode(Bin), Model);
    Error ->
      Error
  end.


%%----------------------------------------------------------------------
%% Function: simple_form/2
%% Purpose: translate an XML document to 'simple form'.
%% Args:
%%     Xml: an XML document (encoded binary or list or string())
%%     Options: [Option]
%%     Option: {nameFun, NameFun} |
%%             {output_encoding, Encoding}
%%
%%  Namefun is a function with 3 arguments: Name, Namespace, Prefix.
%%  It should return a term. It is called for each tag and antribute
%%  name. The result will be used in the output. Default is Name
%%  if Namespace == undefined, and a string {Namespace}Name otherwise.
%%
%%  See parse_sax() for a description of the output_encoding option.
%%
%% Returns: {ok, SimpleForm, Tail}
%%     or {error, ErrorMessage}.
%%----------------------------------------------------------------------
simple_form(Xml, Options) ->
  erlsom_simple_form:scan(Xml, Options).

simple_form(Xml) ->
  simple_form(Xml, []).

simple_form_file(File) ->
  simple_form_file(File, []).

simple_form_file(File, Options) ->
  case file:read_file(File) of
    {ok, Bin} ->
      simple_form(Bin, Options);
    Error ->
      Error
  end.


%%----------------------------------------------------------------------
%% Function: write/2
%% Purpose: translate a structure of records to an XML document. This is the
%%     inverse of erlsom:parse(). The XML will conform to an XSD, provided
%%     that the input structure matches with this XSD.
%%     (The XSD is represented by the 'Model', the result of the translation
%%     of the XSD by erlsom:compile().)
%% Args:
%%     Struct: a structure or records that represents the XML document. See
%%     the documentation for the mapping.
%%     Model = the internal representation of the XSD; the result of
%%     erlsom:compile().
%%     Options: [{output, list | charlist | binary}]. In case the option 'list' is
%%     selected (this is the default), the output will be a list of unicode
%%     code points. In case the option 'charlist' is selected, the output will
%%     be a charlist, i.e. a deep lists of numbers representing Unicode
%%     code points and UTF-8 encoded binaries. If 'binary' is selected the
%%     output will be a UTF-8 encoded binary.
%%
%% Returns: {ok, Document} where Document is an XML document (a string),
%%     or {error, ErrorMessage}.
%%----------------------------------------------------------------------
write(Struct, Model) ->
  erlsom:write(Struct, Model, []).

write(Struct, Model, Options) ->
  erlsom_write:write(Struct, Model, Options).


%%----------------------------------------------------------------------
%% Function: parse_sax/3
%% Purpose: parse an XML document, using the org.xml.sax ContentHandler
%%     interface [SAX].
%%
%% Args:
%%     Xml - A list of integers that correspond with the characters in an XML
%%         document. Can be either 1 byte characters or integers that
%%         correspond to Unicode code points.
%%
%%     State - a term() that is passed to the EventFun.
%%
%%     Eventfun - a fun() that is called by the parser whenever it has parsed
%%         a bit of the Xml input. The function is called by the parser
%%         according to the Sax specification (see [SAX]).
%%
%%         EventFun should accept the following arguments:
%%         - Event, a tuple that describes the event, see erlsom_sax for a
%%           description of the events
%%         - State - a term()
%%
%%         EventFun should return State, a term() that will be passed back to
%%         the next invocation of EventFun.
%%
%%     Options - [Option]
%%     Option
%%       - {output_encoding, Encoding} This determines the encoding of
%%         the 'character data': element values and attribute values. The
%%         only supported encoding at this moment is 'utf8'. The default is
%%         string().
%%
%%  Returns: {ok, State, TrailingCharacters}
%%     State = the result of the last invocation of the callback function)
%%     TrailingCharacters = any characters in the input string after the
%%       XML document.
%%----------------------------------------------------------------------
parse_sax(Xml, State, EventFun, Options) ->
  erlsom_sax:parseDocument(Xml, State, EventFun, Options).

parse_sax(Xml, State, EventFun) ->
  parse_sax(Xml, State, EventFun, []).

%%----------------------------------------------------------------------
%% Function: sax/3
%% Deprecated. Same as 'parse_sax/3', but without the trailing
%% characters. If there are any trailing characters they are ignored.
%%----------------------------------------------------------------------

sax(Xml, State, EventFun) ->
  sax(Xml, State, EventFun, []).

sax(Xml, State, EventFun, Options) ->
  {ok, Result, _TrailingCharacters} = parse_sax(Xml, State, EventFun, Options),
  Result.

write_hrl(Model, Output) ->
  write_hrl(Model, Output, []).

write_hrl(Model, Output, Options) ->
  Hdr = erlsom_writeHrl:writeHrl(Model, Options),
  file:write_file(Output, Hdr).

%%----------------------------------------------------------------------
%% Function: write_xsd_hrl_file/3
%% Purpose: write record definitions (a .hrl file) for an xsd.
%%
%% Args:
%%     Xsd = the filename of the .xsd file
%%
%%     Namespaces = [#ns]
%%
%%     'Namespaces' should include the URIs of all namespaces used
%%     in the XSD. 'Prefix' is prefixed to the record names.
%%
%%     Output = the name of the output file.
%%
%%     Options = Compile options plus the following:
%%
%%        * `{attribute_hrl_prefix, string()}' -- prefix for the record
%%          fields representing attributes. Defaults to "". E.g. if option
%%          {attribute_hrl_prefix, "attr_"} will be passed to this function,
%%          attribute "id" in the XML Schema will be represented by the field
%%          'attr_id' in the generated record. This is useful in the cases
%%          when a complex type have an attribute and an element with the
%%          same name.
%%
%% Returns: ok, or an error if the file was not found.
%%----------------------------------------------------------------------
write_xsd_hrl_file(Xsd, Output) ->
  write_xsd_hrl_file(Xsd, Output, []).

write_xsd_hrl_file(Xsd, Output, Options) ->
  case file:read_file(Xsd) of
    {ok, Bin} ->
      Hdr = erlsom_writeHrl:writeXsdHrlFile(erlsom_lib:toUnicode(Bin), Options),
      file:write_file(Output, Hdr);
    Error ->
      Error
  end.

write_hrl_file(Xsd, Output) ->
  write_hrl_file(Xsd, "p", [], Output).

write_hrl_file(Xsd, Prefix, Output) ->
  write_hrl_file(Xsd, Prefix, [], Output).

write_hrl_file(Xsd, Prefix, Namespaces, Output) ->
  case file:read_file(Xsd) of
    {ok, Bin} ->
      Hdr = erlsom_writeHrl:writeHrlFile(erlsom_lib:toUnicode(Bin), Prefix, Namespaces),
      file:write_file(Output, Hdr);
    Error ->
      Error
  end.

add_xsd_file(XsdFile, Options, Model) ->
  case file:read_file(XsdFile) of
    {ok, Bin} ->
      {ok, erlsom_add:add(erlsom_lib:toUnicode(Bin), Options, Model)};
    Error ->
      Error
  end.

add_file(XsdFile, Prefix, Model) ->
  {_, Result} = add_xsd_file(XsdFile, [{prefix, Prefix}], Model),
  Result.

%% add the model for XML schema to a model.
%% We need a special function, since:
%% A - Erlsom can't parse the schema for XML schema
%% B - even if it would be able to parse the schema for XML schema,
%%     the output would be difficult to process.
%%
%% Expected to be used for parsing of WSDL files.
add_xsd_model(Model) ->
  erlsom_add:add_xsd_model(Model).

%% add Model2 to Model1
add_model(Model1, Model2) ->
  erlsom_add:add_model(Model1, Model2).
