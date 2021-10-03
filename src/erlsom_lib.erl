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
%%% A couple of support functions for Erlsom
%%% ====================================================================

-module(erlsom_lib).

-export([convertPCData/4,
         makeName/2, makeName/4, nameWithoutPrefix/1,
         makeAttrName/2, makeTypeName/4, makeTypeName/2,
         makeElementName/2, makeGroupName/2, makeTypeRef/3,
         makeTypeRefAtom/2, makeGroupRef/2, makeElementRef/2,
         makeAttrRef/2, makeTagFromRef/2,
         makeTag/2, makeTag/3,
         findPrefix/2, findPrefix2/2,
         translateType/2,
         minMax/1, multMinMax/2,
         tagNamespace/2,
         listLength/1,
         prettyPrint/1,
         xmlString/1,
         strip/1,
         toUnicode/1, detect_encoding/1, detectEncoding/3,
         findFile/4, find_xsd/4, findType/6, findType/2,
         readImportFile/1,
         newTree/0, addTreeElement/3, isAncestor/3, getAncestor/2,
         getLeaves/2,
         getDescendants/2,
         documentAlternatives/1,
         emptyListIfUndefined/1,
         searchBase/2,
         makeQname/1, localName/1,
         getUriFromQname/1,
         getTargetNamespaceFromXsd/1,
         removePrefixes/1, unique/1,
         check_int/2,
         getTypeFromElement/2,
         getNamespacesFromModel/1, getPrefixFromModel/2]).

-include("erlsom_compile.hrl").
-include("erlsom_sax.hrl").
-include("erlsom.hrl").
-include("erlsom_parse.hrl"). %% the record definitions

-define(SPACE, 32).

%% debug(Text) -> io:format("lib: ~p\n", [Text]).

%%debug(Text1, Text2) ->
  %%io:format("~p ~p\n", [Text1, Text2]).


%% Convert text to the indicated type.
convertPCData(Text, Type, Namespaces, NamespaceMapping) ->
  try
    convertPCData2(Text, Type, Namespaces, NamespaceMapping)
  catch
    _:_ ->
      throw({error,
             lists:flatten(io_lib:format("Invalid value for type ~p : ~p", [Type, Text]))})
  end.

convertPCData2(Text, char, _Namespaces, _NamespaceMapping) when is_binary(Text) ->
  Text;
convertPCData2(Text, Type, Namespaces, NamespaceMapping) when is_binary(Text) ->
  convertPCData2(erlsom_ucs:decode_utf8(Text), Type, Namespaces, NamespaceMapping);
convertPCData2(Text, Type, Namespaces, NamespaceMapping) ->
  case Type of
    char ->
      Text;
    atom ->
      list_to_atom(Text);
    ascii ->
      %% this is only used by the compiler. Names (which have to be converted to atoms later on
      %% in the process) have this type.
      try
        list_to_atom(Text)
      catch
        _Class:Exception -> throw(Exception)
      end,
      Text;
    integer ->
      list_to_integer(Text);
    bool ->
      case Text of
        "true" -> true;
        "false" -> false;
        "0" -> false;
        "1" -> true;
        _ -> throw({error, "invalid value for boolean: " ++ Text})
      end;
    float ->
      xml_to_float(strip(Text));
    qname ->
      %% qname has form prefix:localname (or, if there is no prefix: localname)
      %% split the two parts, look up the prefix to find the uri, and put it into
      %% a qname record {localname, URI, prefix} (URI = undefined if there was no prefix)
      {Prefix, LocalName} = splitOnColon(Text),
      case lists:keysearch(Prefix, 3, Namespaces) of
        {value, #ns{uri = URI}} ->
          %% this is namespace qualified - now see whether a mapping applies
          case lists:keysearch(URI, 2, NamespaceMapping) of
            {value, #ns{prefix = MappedPrefix}}  ->
              #qname{localPart = LocalName, uri = URI, prefix = Prefix,
                     mappedPrefix = MappedPrefix};
            _Else ->
              #qname{localPart = LocalName, uri = URI, prefix = Prefix, mappedPrefix = Prefix}
          end;
        _Else ->
          if
            Prefix == [] -> %% no prefix, no default namespace
              #qname{localPart = Text};
            Prefix == "xml"  -> %% by convention
              #qname{localPart = LocalName, uri = "http://www.w3.org/XML/1998/namespace",
                     prefix = Prefix, mappedPrefix = Prefix};
            true ->
              throw({error, invalid})
          end
      end;
    {integer, _} ->
      convert_integer(Type, Text)
  end.


xml_to_float("NaN") ->
  'NaN';
xml_to_float("INF") ->
  'INF';
xml_to_float("+INF") ->
  'INF';
xml_to_float("-INF") ->
  '-INF';
xml_to_float(Float) ->
  try
    list_to_float(Float)
  catch
    %% "10" is a problem
    error:badarg ->
      try
        list_to_integer(Float) * 1.0
      catch
        %% "10E3" is also a problem
        error:badarg ->
          case string:tokens(Float, "Ee") of
            [Mantissa, Exponent] ->
              list_to_integer(Mantissa) *
                math:pow(10, list_to_integer(Exponent))
          end
      end
  end.


%% Tree is a data structure used to find out the relations between types.
%% It is actually a forest, in the sense that there doesn't have to be
%% a unique root.
newTree() -> [].

addTreeElement(Child, Parent, Tree) ->
  [{Child, Parent} | Tree].

%% find out whether Ancestor is really an Ancestor of Element.
isAncestor(Element, Element, _Tree) -> true; %% added because of problem reported by Stu
isAncestor('#ANY', _Element, _Tree) -> true; %% issue 81
isAncestor(Ancestor, Element, Tree) ->
  case lists:keysearch(Element, 1, Tree) of
    {value, {_, Ancestor}} -> true;
    {value, {_, Parent} = Elem} ->
      %% remove the element, just to be sure that we don't end up in an
      %% endless loop.
      isAncestor(Ancestor, Parent, lists:delete(Elem, Tree));
    _ -> false
  end.

%% get the ancestor of this type (if any)
getAncestor(Element, Tree) ->
  case lists:keysearch(Element, 1, Tree) of
    {value, {_, Ancestor}} -> {value, Ancestor};
    _ -> false
  end.

%% get all leaves (non-abstract types) of node (abstract type)
getLeaves(Node, Tree) ->
  Children = [Child || {Child, N} <- Tree, N == Node],
  getLeaves(Children, [], Tree).

getLeaves([], Leaves, _) ->
  Leaves;
getLeaves([Node | T], Leaves, Tree) ->
  Children = [Child || {Child, N} <- Tree, N == Node],
  %% if Node has no children, it is a leave.
  case Children of
    [] ->
      getLeaves(T, [Node | Leaves], Tree);
    _ ->
      %% If Node does have children, go one level deeper
      %% Delete (one instance..) of Node from the tree, to prevent
      %% endless looping
      getLeaves(Children ++ T, Leaves, lists:keydelete(Node, 2, Tree))
  end.

%% get all descendants of Node
getDescendants(Node, Tree) ->
  Children = [Child || {Child, N} <- Tree, N == Node],
  getDescendants(Children, Children, Tree).

getDescendants([], Descendants, _) ->
  Descendants;
getDescendants([Node | T], Descendants, Tree) ->
  Children = [Child || {Child, N} <- Tree, N == Node],
  %% if Node has no children, it is a leave.
  case Children of
    [] ->
      getDescendants(T, Descendants, Tree);
    _ ->
      %% If Node does have children, go one level deeper
      %% Delete (one instance..) of Node from the tree, to prevent
      %% endless looping
      getDescendants(Children ++ T, Children ++ Descendants, lists:keydelete(Node, 2, Tree))
  end.

minMax(undefined) ->
  1;
minMax("unbounded") ->
  unbound;
minMax(Integer) ->
  list_to_integer(Integer).

multMinMax(0, _) -> 0;
multMinMax(_, 0) -> 0;
multMinMax(unbound, _) -> unbound;
multMinMax(_, unbound) -> unbound;
multMinMax(A, B) -> A * B.

%% returns the URI that belongs to a tag.
%% tag is an atom 'ppp:llll' or 'llll' (ppp = prefix, llll  = local name)
%% namespaces is [#ns{prefix, uri}]
tagNamespace(Tag, Namespaces) ->
  tagNamespace(atom_to_list(Tag), [], Namespaces).

tagNamespace([$: | _Tail], Acc, Namespaces) ->
  case lists:keysearch(lists:reverse(Acc), #ns.prefix, Namespaces) of
    {value, #ns{uri = Uri}} ->
      Uri;
    _Other ->
      undefined
  end;
tagNamespace([], _Acc, _Namespaces) ->
  undefined;
tagNamespace([Char | Tail], Acc, Namespaces) ->
  tagNamespace(Tail, [Char | Acc], Namespaces).


nameWithoutPrefix(Name) ->
  nameWithoutPrefix(Name, []).

nameWithoutPrefix([$: | Tail], _Acc) ->
  Tail;
nameWithoutPrefix([Char | Tail], Acc) ->
  nameWithoutPrefix(Tail, [Char | Acc]);
nameWithoutPrefix([], Acc) ->
  lists:reverse(Acc).

%% 'strict' == false
translateType(String, false) ->
  case String of
    "integer" ->
       'integer';
    "int" ->
       'integer';
    "QName" ->
       'qname';
    "boolean" ->
       'bool';
    _Else ->
       'char'
  end;
%% strict == true
translateType(String, true) ->
  case String of
    "integer" ->
       integer;
    "long" ->               % -9223372036854775807 =<  X =< 9223372036854775808
       {integer, long};
    "int" ->                % -2147483648 =<  X =< 2147483647
       {integer, int};
    "short" ->              % -32768 =< X =< 32767
       {integer, short};
    "byte" ->
       {integer, byte};                % -128 =< X =< 127
    "QName" ->
       qname;
    "boolean" ->
       bool;
    "nonNegativeInteger" -> % X >= 0
      {integer, nonNegativeInteger};
    "positiveInteger" ->    % X > 0
      {integer, positiveInteger};
    "unsignedLong" ->       % 0 =< X =< 18446744073709551615
      {integer, unsignedLong};
    "unsignedInt" ->        % 0 =< X =<  4294967295
      {integer, unsignedInt};
    "unsignedShort" ->      % 0 =< X =<  65535
      {integer, unsignedShort};
    "unsignedByte" ->       % 0 =< X =<  255
      {integer, unsignedByte};
    "nonPositiveInteger" -> % X =< 0
      {integer, nonPositiveInteger};
    "negativeInteger" ->    % X < 0
      {integer, negativeInteger};
    "float" ->
      float;
    % no distinction between double and float - both are mapped to
    % erlang float, no chcecks on size will be performed
    "double" ->
      float;
    _Else ->
       'char'
  end.


findPrefix(undefined, _Namespaces) ->
  [];
findPrefix(Namespace, Namespaces) ->
  case lists:keysearch(Namespace, #ns.uri, Namespaces) of
    {value, #ns{prefix = undefined}} ->
      "";
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":";
    _Else ->
      throw({error, "Namespace not found " ++ Namespace})
  end.

findPrefix2(undefined, _Namespaces) ->
  [];
findPrefix2(Namespace, Namespaces) ->
  case lists:keysearch(Namespace, #ns.uri, Namespaces) of
    {value, #ns{prefix = undefined}} ->
      "";
    {value, #ns{prefix = Prefix}} ->
      Prefix;
    _Else ->
      "P"
  end.

makeTypeName(Name, Prefix) ->
  TypePrefix = case get(erlsom_typePrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  Prefix ++ TypePrefix ++ Name.

makeGroupName(Name, Prefix) ->
  TypePrefix = case get(erlsom_groupPrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  Prefix ++ TypePrefix ++ Name.

makeElementName(Name, Prefix) ->
  %% TypePrefix = case get(erlsom_elementPrefix) of
                 %% undefined -> "";
                 %% Value -> Value
               %% end,
  Prefix ++ Name.

makeTypeName(NameInXsd, ElementFormDefault, Path, Prefix) ->
  TypePrefix = case get(erlsom_typePrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  makeName(NameInXsd, ElementFormDefault, TypePrefix ++ Path, Prefix).

makeName(NameInXsd, _ElementFormDefault = "qualified", Path, Prefix) ->
   Prefix ++ Path ++ NameInXsd;

makeName(NameInXsd, _ElementFormDefault, Path, _Prefix) ->
   Path ++ NameInXsd.

%% -record(schemaInfo, {targetNamespace, elementFormDefault, namespacePrefix, namespaces}).
makeName(NameInXsd, #schemaInfo{elementFormDefault="qualified", targetNamespace=TNS, namespaces=NS,
                                path=Path}) ->

  %% find the target namespace in NS,
  %% add the prefix and the path

  case NameInXsd of
    _ -> ok
  end,
  case lists:keysearch(TNS, 2, NS) of
    {value, #ns{prefix = undefined}} ->
      Path ++ NameInXsd;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ Path ++ NameInXsd;
    _Else ->
      if
        TNS == undefined ->
          Path ++ NameInXsd;
        true ->
          throw({error, "Namespace not found " ++ NameInXsd})
      end
  end;

makeName(NameInXsd, #schemaInfo{targetNamespace=TNS, namespaces=NS, path=[]}) ->
  %% elementFormDefault = unqualified (or undefined)
  %% since Path = [], this is a global element,and we need to add the prefix.

  case lists:keysearch(TNS, 2, NS) of
    {value, #ns{prefix = undefined}} ->
      NameInXsd;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ NameInXsd;
    _Else ->
      if
        TNS == undefined ->
          NameInXsd;
        true ->
          throw({error, "Namespace not found " ++ NameInXsd})
      end
  end;

makeName(NameInXsd, #schemaInfo{targetNamespace=TNS, namespaces=NS, path=Path}) ->
  %% elementFormDefault = unqualified (or undefined)
  %% since Path /= [], this is a local element,and we need to add the prefix and the path.

  case lists:keysearch(TNS, 2, NS) of
    {value, #ns{prefix = undefined}} ->
      NameInXsd;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ Path ++ NameInXsd;
    _Else ->
      if
        TNS == undefined ->
          Path ++ NameInXsd;
        true ->
          throw({error, "Namespace not found " ++ NameInXsd})
      end
  end.

%% -record(schemaInfo, {targetNamespace, elementFormDefault, namespacePrefix, namespaces}).
makeAttrName(NameInXsd, _Info) ->
  NameInXsd.

makeTypeRefAtom(Qname, Namespaces) ->
  %% This is only used when building the type-hierarchy. In that
  %% context 'strict' is not relevant (so we can simply put 'false').
  TypeRef = makeTypeRef(Qname, Namespaces, false),
  case TypeRef of
    {_, _} -> TypeRef;
    _ -> list_to_atom(TypeRef)
  end.

%% makeTypeRef creates a reference to a type. This can either be a type
%% defined in the XSD (or an imported XSD), or a predefined type (like
%% xsd:string).
%% For the predefined types special codes are returned ({'#PCDATA', ...}).
%% input is a qname.
%% The output includes the prefix (as found in the Namespaces list), unless the
%% type is not in a particular namespace.
%% If a special prefix was defined for types, this prefix is also added (after
%% the namespace prefix. So the result could be P:t#MyType, for example.
%% The 'type-prefix' is taken from a proces-variable (not very nice, sorry).

%% TODO: should return an atom (or {'PCDATA', ...})?
makeTypeRef(undefined, _, _) ->
  %% the 'ur-type': any type (and any attribute).
  '#ANY';

makeTypeRef(Qname = #qname{uri = NS, localPart = Local}, Namespaces, Strict) ->
  TypePrefix = case get(erlsom_typePrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  case NS of
    "http://www.w3.org/2001/XMLSchema" ->
      {'#PCDATA', translateType(Local, Strict)};
    _Else ->
      makeRef(Qname, Namespaces, TypePrefix)
  end.

makeElementRef(Qname, Namespaces) ->
  %% ElementPrefix = case get(erlsom_elementPrefix) of
                 %% undefined -> "";
                 %% Value -> Value
               %% end,
  makeRef(Qname, Namespaces, "").

makeGroupRef(Qname, Namespaces) ->
  GroupPrefix = case get(erlsom_groupPrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  makeRef(Qname, Namespaces, GroupPrefix).

makeAttrRef(QName, Namespaces) ->
  makeRef(QName, Namespaces, "").

makeTagFromRef(QName, Namespaces) ->
  makeRef(QName, Namespaces, "").

%% ExtraPrefix is the additional prefix to distinguish
%% types, groups and elements
makeRef(#qname{uri = NS, localPart = Local}, Namespaces, ExtraPrefix) ->
  case lists:keysearch(NS, 2, Namespaces) of
    {value, #ns{prefix = undefined}} ->
      ExtraPrefix ++ Local;
    %% undefined ~~ ""
    {value, #ns{prefix = ""}} ->
      ExtraPrefix ++ Local;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ ExtraPrefix ++ Local;
    _ ->
      if
        NS == undefined ->
          ExtraPrefix ++ Local;
        true ->
          case {NS, Local} of
            %% weird cases
            {"http://www.w3.org/XML/1998/namespace", "lang"} -> "xml:lang";
            {"http://www.w3.org/XML/1998/namespace", "space"} -> "xml:space";
            {"http://www.w3.org/XML/1998/namespace", "base"} -> "xml:base";
            {"http://www.w3.org/XML/1998/namespace", "id"} -> "xml:id";
            _ ->
              throw({error, "Namespace not found " ++ NS})
          end
      end
  end.


makeTag(NameInXsd, Prefix, _ElementFormDefault = "qualified") ->
      Prefix ++ NameInXsd;
makeTag(NameInXsd, _Prefix, _ElementFormDefault) ->
      NameInXsd.

makeTag(NameInXsd, #schemaInfo{targetNamespace=undefined}) ->
  NameInXsd;

makeTag(NameInXsd, #schemaInfo{elementFormDefault="qualified", targetNamespace=TNS, namespaces=NS}) ->
  %% find the target namespace in NS,
  %% add the prefix.

  case lists:keysearch(TNS, 2, NS) of
    {value, #ns{prefix = undefined}} ->
      NameInXsd;
    {value, #ns{prefix = Prefix}} ->
      Prefix ++ ":" ++ NameInXsd;
    _Else ->
      if
        TNS == undefined ->
          NameInXsd;
        true ->
          throw({error, "Namespace not found " ++ TNS})
      end
  end;

makeTag(NameInXsd, _SchemaInfo) ->
  %% defaultElementForm = unqualified, do not add the prefix (TODO: except for global elements and types).
  NameInXsd.

listLength(undefined) -> 0;
listLength(List) -> length(List).

toUnicode(Bin) ->
  autodetect(Bin).

findType(TypeReference, Types, Attributes, TypeHierarchy, Namespaces, NamespaceMapping) ->
    case findXsiType(Attributes) of
      {value, XsiType} ->
        findDerivedType(TypeReference, XsiType, Types, TypeHierarchy, Namespaces, NamespaceMapping);
      _ -> 
        findType(TypeReference, Types)
    end.

%% If this is an anyType and no xsi:type attribute has been provided, assume that the value is a string.
%% This is how it used to work in earlier versions (before issue 81), so for backwards compatibility
%% this behaviour should continue to work. 
findType('#ANY', _Types) ->
  {'#PCDATA', char};
findType(TypeReference, Types) ->
  case lists:keysearch(TypeReference, #type.nm, Types) of
    {value, Value} -> Value;
    _Else ->
      throw({error, "Type not found: " ++ atom_to_list(TypeReference)})
  end.

findXsiType([]) -> false;
findXsiType([#attribute{localName= "type",
                        uri = "http://www.w3.org/2001/XMLSchema-instance",
                        value = Value}| _Tail]) -> {value, Value};
findXsiType([_| Tail]) ->
  findXsiType(Tail).

findDerivedType(Type, XsiType, Types, TypeHierarchy, Namespaces, NamespaceMapping) ->
  #qname{localPart = LocalName, mappedPrefix = MappedPrefix, uri = TypeNS} =
    convertPCData(XsiType, qname, Namespaces, NamespaceMapping),
  XsiTypeMapped = list_to_atom(makeTypeName(LocalName, case MappedPrefix of undefined -> ""; "" -> ""; _ -> MappedPrefix ++ ":" end)),
  case isAncestor(Type, XsiTypeMapped, TypeHierarchy) of
    false ->
      throw({error, "Type not found in hierarchy: " ++ atom_to_list(Type)});
    _ ->
      case lists:keysearch(XsiTypeMapped, #type.nm, Types) of
        {value, Value} ->
          Value;
        _ ->
          case TypeNS of
            "http://www.w3.org/2001/XMLSchema" ->
              {'#PCDATA', char};
            _ ->
          throw({error, "Derived type not found: " ++ atom_to_list(Type)})
      end
      end
  end.

autodetect(Input) ->
  case detectEncoding(Input) of
    ucs4be ->
      xmerl_ucs:from_ucs4be(Input);
    ucs4le ->
      xmerl_ucs:from_ucs4le(Input);
    utf16be ->
      {Result, []} = erlsom_ucs:from_utf16be(Input),
      Result;
    utf16le ->
      {Result, []} = erlsom_ucs:from_utf16le(Input),
      Result;
    utf8 ->
      {Result, []} = erlsom_ucs:from_utf8(Input),
      Result;
    iso_8859_1 ->
      binary_to_list(Input)
  end.


%% CFun = {ContinuationFunction, ContinuationState} (or undefined)
detectEncoding(Xml, _CFun, CState) when is_list(Xml) ->
  {list, Xml, CState};
detectEncoding(Xml, CFun, CState) when is_binary(Xml) ->
  case Xml of
    <<>> ->
      case CFun(Xml, CState) of
        {<<>>, _} ->
          throw({error, "empty document"});
        {Xml2, State2} ->
          {detectEncoding(Xml2), Xml2, State2}
      end;
    _ ->
      {detectEncoding(Xml), Xml, CState}
  end.

%%------------------------------------------------------------------------------
%% This was copied from xmerl_lib (by Ulf Wiger), but modified to work
%% on binaries in stead of lists. I also removed the option to specify
%% a character set - the function only looks at the first 2 or 4 bytes.
%% Finally, I changed it to not remove the byte order mark (since
%% erlsom has no problem with the byte order mark).
%%
%% Auto detect what kind of character set we are dealing with and transform
%% to Erlang integer Unicode format if found.
%% Appendix F, Page 56-57, XML 1.0 W3C Recommendation 6 October 2000
%% (http://www.w3.org/TR/REC-xml)
%% 00 00 00 3C ( "<" in UCS-4 big-endian)
%% 3C 00 00 00 ( "<" in UCS-4 little-endian)
%% FE FF (UTF-16 - big-endian Mark)
%% FF FE (UTF-16 - little-endian Mark)
%% 00 3C 00 3F ( "<?" in UTF-16 big-endian)
%% 3C 00 3F 00 ( "<?" in UTF-16 big-endian)
%% 3C 3F (7-bit,8-bit or mixed width encoding)
%% 4C 6F A7 94 (EBCDIC) - Not Implemented!!!!

%% Check byte-order mark and transform to Unicode, Erlang integer
%%% --- With byte-order mark
detect_encoding(List)
  when is_list(List) ->
  detect_encoding(list_to_binary(List));
detect_encoding(Xml) ->
  {detectEncoding(Xml), Xml}.


%% This is the internal version
%% the version detect_encoding() is there for reasons of backward
%% compatibility
detectEncoding(<<0,0,16#fe,16#ff, _Rest/binary>>) ->
  ucs4be;
detectEncoding(<<16#ff,16#fe,0,0, _Rest/binary>>) ->
  ucs4le;
detectEncoding(<<16#fe,16#ff, _Rest/binary>>) ->
  utf16be;
detectEncoding(<<16#ff,16#fe, _Rest/binary>>) ->
  utf16le;

detectEncoding(<<16#ef,16#bb,16#bf, _Rest/binary>>) ->
  utf8;

%%% --- Without byte-order mark
detectEncoding(<<0,0,0,16#3c, _Rest/binary>>) ->
  ucs4be;
detectEncoding(<<16#3c,0,0,0, _Rest/binary>>) ->
  ucs4le;

detectEncoding(<<0,16#3c,0,16#3f, _Rest/binary>>) ->
  utf16be;
detectEncoding(<<16#3c,0,16#3f,0, _Rest/binary>>) ->
  utf16le;

%% based on a suggestion by Tobbe:
detectEncoding(<<$<,$?,$x,$m,$l, _Rest/binary>> = Content) ->
  detect_encoding2(Content);

detectEncoding(_Input) ->
  %% debug("unknown encoding? Assume UTF-8"),
  utf8.

detect_encoding2(<<First100:100/binary, _Rest/binary>>) ->
  Variables = parse_prolog(binary_to_list(First100)),
  detect_encoding3(Variables);

detect_encoding2(Content) -> %% < 100 characters in the XML
  Variables = parse_prolog(binary_to_list(Content)),
  detect_encoding3(Variables).

detect_encoding3(Variables) ->
  case lists:keysearch("encoding", 1, Variables) of
    {value, {_, Encoding}} ->
      case encoding_type(Encoding) of
        'utf-8' ->
          utf8;
        'iso-8859-1' ->
          iso_8859_1;
        'iso-8859-15' ->
          iso_8859_15;
        'us-ascii' ->
          iso_8859_1;
        _ -> throw({error, "Encoding " ++ Encoding ++ " not supported"})
      end;
    _ ->
      utf8
  end.

encoding_type(Cs) when is_list(Cs) ->
   case to_lower(Cs) of
       "iso-8859-1" -> 'iso-8859-1';
       "iso_8859_1" -> 'iso-8859-1';
       "iso_8859-1" -> 'iso-8859-1';
       "iso8859-1"  -> 'iso-8859-1';
       "iso-8859-15" -> 'iso-8859-15';
       "iso_8859_15" -> 'iso-8859-15';
       "iso_8859-15" -> 'iso-8859-15';
       "iso8859-15"  -> 'iso-8859-15';
       "utf-8"      -> 'utf-8';
       "utf_8"      -> 'utf-8';
       "us-ascii"   -> 'us-ascii';
       _            -> false
   end.

to_lower(Str) when is_list(Str)   -> [to_lower(C) || C <- Str];
to_lower(C) when C >= $A, C =< $Z -> C+($a-$A);
to_lower(C)                       -> C.
%% end of the code based on Tobbes code

%% some code to parse the XML prolog.
%% returns a list of tuples {variable, value}
%% <?xml version = '1.0' encoding="utf-9" ?>
%% gives [{"version", "1.0"}, {"encoding", "utf-9"}]

parse_prolog("<?xml" ++ Tail) ->
  parse_variables(Tail, []);

parse_prolog(_) -> [].

parse_variables([], _Acc) ->
  %% error, but never mind
  [];

parse_variables("?>" ++ _, Acc) -> Acc;

parse_variables(T = [X | Tail], Acc) ->
  case typeOfMarkupChar(X) of
    namestartchar ->
      parse_variable(T, [], Acc);
    whitespace ->
      parse_variables(Tail, Acc);
    _ -> []
  end.

parse_variable([X| Tail], NameAcc, Acc) ->
  case typeOfNameChar(X) of
    namechar -> parse_variable(Tail, [X | NameAcc], Acc);
    whitespace -> parse_variable_is(Tail, lists:reverse(NameAcc), Acc);
    equalsign -> parse_to_quote(Tail, lists:reverse(NameAcc), Acc);
    _ -> []
  end.

parse_variable_is([X | Tail], Name, Acc) ->
  case typeOfNameChar(X) of
    whitespace -> parse_variable_is(Tail, Name, Acc);
    equalsign -> parse_to_quote(Tail, Name, Acc);
    _ -> []
  end.

parse_to_quote([X|Tail], Name, Acc) ->
  case typeOfMarkupChar(X) of
    whitespace -> parse_to_quote(Tail, Name, Acc);
    quote -> parse_value(Tail, [], Name, X, Acc);
    _ -> []
  end.

parse_value([Quote|Tail], ValueAcc, Name, Quote, Acc) ->
  parse_variables(Tail, [{Name, lists:reverse(ValueAcc)} | Acc]);
parse_value([X|Tail], ValueAcc, Name, Quote, Acc) ->
  parse_value(Tail, [X | ValueAcc], Name, Quote, Acc);
parse_value([], _ValueAcc, _Name, _Quote, _Acc) ->
  [].


findFile(Namespace, Location, IncludeFiles, IncludeDirs) ->
  case lists:keysearch(Namespace, 1, IncludeFiles) of
    {value, {_, Prefix, Schema = #schemaType{}}} ->
      {Schema, Prefix};
    {value, {_, Prefix, undefined}} ->
      {getFile(Location, IncludeDirs), Prefix};
    {value, {_, Prefix, FileName}} ->
      {getFile(FileName, IncludeDirs), Prefix};
    _ ->
      {getFile(Location, IncludeDirs), undefined}
  end.

getFile("http://"++_ = URL, _) ->
  httpGetFile(URL);
getFile("https://"++_ = URL, _) ->
  httpGetFile(URL);
getFile(Location, IncludeDirs) ->
  case filelib:is_file(Location) of
    true ->
      readImportFile(Location);
    _ ->
      %% debug(IncludeDirs),
      findFileInDirs(Location, IncludeDirs)
  end.

httpGetFile(URL) ->
  case httpc:request(get, {URL, [{"user-agent", "erlsom"}]}, [], []) of
        {ok,{{_HTTP,200,_OK}, _Headers, Body}} ->
            toUnicode(Body);
        {ok,{{_HTTP,RC,Emsg}, _Headers, _Body}} ->
            error_logger:error_msg("~p: http-request got: ~p~n",
                [?MODULE, {RC, Emsg}]),
            {error, "failed to retrieve: "++URL};
        {error, Reason} ->
            error_logger:error_msg("~p: http-request failed: ~p~n",
                [?MODULE, Reason]),
            {error, "failed to retrieve: "++URL}
  end.

findFileInDirs(undefined, []) ->
  throw({error, "Include file not found (undefined)"});
findFileInDirs(Location, []) ->
  throw({error, "Include file not found " ++ Location});

findFileInDirs(Location, [H | T]) ->
  Name = filename:join([H, Location]),
  case filelib:is_file(Name) of
    true ->
      readImportFile(Name);
    _ ->
      findFileInDirs(Location, T)
  end.

readImportFile(Name) ->
  case file:read_file(Name) of
    {ok, Bin} ->
      toUnicode(Bin);
    Error ->
      throw({error,
            lists:flatten(io_lib:format("Error reading include file ~p - ~p",
                                        [Name, Error]))})
  end.

%% Include_fun is a function that finds the files that are included or imported in
%% the XSD. It should be a function that takes 4 arguments:
%%        - Namespace (from the XSD). This is a string or 'undefined'
%%        - SchemaLocation (from the XSD). This is a string or 'undefined'
%%        - Include_dirs. This is the value of the Include_dirs option if this option
%%          was passed to compile_xsd(); 'undefined' otherwise.
%%        - Inlcude_list. This is the value of the Include_list option if this
%%          option was passed to compile_xsd(); 'undefined' otherwise.
%%
%% Include_fun should return {XSD, Prefix}, where XSD is a XSD = string(), Prefix
%% = string or 'undefined', see above.
find_xsd(Namespace, Location, Include_dirs, Include_list) ->
  case get_url(Location) of
    {ok, Body} ->
      Prefix = prefix(Namespace),
      {Body, Prefix};
    _ ->
       erlsom_lib:findFile(Namespace, Location, Include_dirs, Include_list)
  end.

prefix(Namespace) ->
  Tokens = string:tokens(Namespace, "/ "),
  string:substr(lists:last(Tokens), 1, 5).


%%% --------------------------------------------------------------------
%%% Get a file from an URL spec.
%%% function created by Tobbe, copied from yaws_soap_lib
%%% slightly modified
%%% --------------------------------------------------------------------
get_url("http://"++_ = URL) ->
  case httpc:request(URL) of
    {ok, {{_HTTP, 200, _OK}, _Headers, Body}} ->
      {ok, Body};
    _ ->
      {error, "failed to retrieve: "++URL}
  end;
get_url(_) ->
  {error, "not a URL"}.

emptyListIfUndefined(undefined) -> [];
emptyListIfUndefined(List) -> List.

%% Note: initially there can be more than 1 element in the list with the same name (an element and a type).
%% We are looking for the type.
searchBase(_Name, []) ->
  not_found;
searchBase(Name, [H = #typeInfo{typeName = Name, typeType = TypeT} | _])
  when TypeT /= globalElementRefOnly ->
  {value, H};
searchBase(Name, [_H | T]) ->
  searchBase(Name, T).

%% Context can be:
%%  name, markup
%% Returns:
%%  whitespace, lessthan, morethan, etc, see below.
typeOfNameChar(Char) ->
  if
    Char > 96 ->
      if
        Char < 123 -> namechar;
    Char < 127 -> char;
    true -> illegal
      end;
    Char > 64 ->
      if
        Char < 91 -> namechar;
        Char == 95 -> namechar;
    true -> char
      end;
    Char > 47 ->
      if
        Char < 59 -> namechar;
    Char == 61 -> equalsign;
    Char == 62 -> morethan;
    Char == 63 -> questionmark;
    true -> char
      end;
    Char > 32 ->
      if
        Char < 40 -> char;
        Char < 45 -> illegal;
    Char == 47 -> slash;
    true -> namechar
      end;
    Char == 32 -> whitespace;
    Char == 9 -> whitespace;
    Char == 10 -> whitespace;
    Char == 13 -> whitespace;
    true -> illegal
  end.

%% Context can be:
%%  name, markup
%% Returns:
%%  whitespace, lessthan, morethan, etc, see below.
typeOfMarkupChar(Char) ->
  if
    Char == 32 -> whitespace;
    Char > 96 ->
      if
        Char < 123 -> namestartchar;
    Char < 127 -> char;
    true -> illegal
      end;
    Char > 64 ->
      if
        Char < 91 -> namestartchar;
        Char == 95 -> namestartchar;
    true -> char
      end;
    Char > 40 ->
      if
        Char == 47 -> slash;
        Char < 60 -> char;
    Char == 60 -> lessthan;
    Char == 61 -> equalsign;
    Char == 62 -> morethan;
    true -> char
      end;
    Char > 32 ->
      if
        Char == 39 -> quote;
        Char == 34 -> quote;
        Char == 38 -> ampersand;
    true -> char
      end;
    Char == 9 -> whitespace;
    Char == 10 -> whitespace;
    Char == 13 -> whitespace;
    true -> illegal
  end.

%%% hides the definition of #qname{}
makeQname(LocalName) ->
  #qname{localPart = LocalName}.

%%% hides the definition of #qname{}
localName(#qname{localPart = LocalName}) ->
  LocalName.

%%% hides the definition of #qname{}
getUriFromQname(#qname{uri = Uri}) ->
  Uri.

%%% hides the definition of #schemaType{}
getTargetNamespaceFromXsd(#schemaType{targetNamespace = TNS}) ->
  TNS.

%%% hides the definition of #model{}
getPrefixFromModel(#model{nss = Namespaces}, Uri) ->
  case lists:keyfind(Uri, #ns.uri, Namespaces) of
    false ->
      "";
    #ns{prefix = Prefix} ->
      Prefix
  end.

%%% hides the definition of #model{}
getTypeFromElement(Element, Model) ->
  Alternatives = documentAlternatives(Model),
  case lists:keyfind(Element, #alt.tag, Alternatives) of
    #alt{tp = Type} ->
      Type
  end.

getNamespacesFromModel(#model{nss = Namespaces}) ->
  [{Uri, Prefix} || #ns{prefix = Prefix, uri = Uri} <- Namespaces].


%% these are the top-level elements. They can occur in an '#any' type.
%% Check on [], because if for some reason there would be no alternatives
%% there would be an endless loop.
documentAlternatives(#model{tps = [#type{nm = '_document',
                                         els = [#el{alts = Alternatives}]} |
                                                 _]}) when Alternatives /= [] ->
  Alternatives.

%% this is a hack, see erlsom_compile
%% remove the type-prefix from the name
removePrefixes(Name) ->
  {Prefix, LocalName} = splitOnColon(Name),
  %% now remove the typePrefix (if it exists)
  TypePrefix = case get(erlsom_typePrefix) of
                 undefined -> "";
                 Value -> Value
               end,
  {_, WithoutTypePrefix} = lists:split(length(TypePrefix), LocalName),
  case Prefix of
    [] -> WithoutTypePrefix;
    _ -> Prefix ++ ":" ++ WithoutTypePrefix
  end.


%% returns {Prefix, LocalName}
splitOnColon(Text) ->
  PosOfColon = string:chr(Text, $:),
  if
    PosOfColon == 0 ->
      {[], Text};
    true ->
      {string:substr(Text, 1, PosOfColon - 1), string:substr(Text, PosOfColon + 1)}
  end.


%%----------------------------------------------------------------------
%% function : unique/1
%% Arguments: List - [term()]
%% Returns  : [term()]
%% Exception:
%% Effect   : Remove all duplicates from the list.
%%----------------------------------------------------------------------
unique([]) -> [];
unique(List) ->
    Sorted = lists:sort(List),
    unique(hd(Sorted),
       tl(Sorted), []).

unique(A, [A|R], Acc) ->
    unique(A, R, Acc);
unique(A, [B|R], Acc) ->
    unique(B, R, [A|Acc]);
unique(A, [], Acc) ->
    lists:reverse([A|Acc]).


%% NOTE: this probably does not work on all possible XML files, it has only been written
%% to pretty print WSDL files generated by erlsom.
%%
%% The rules are:
%% - first start element is at level 0
%% - level increases with each start element and decreases with each end element
%% - indent level increases for a start element at a lower level
%% - values are always printed directly after the start tag (ignoring mixed for the moment)
%% - endElements that follow value are printed directly after the value
%% - endElements that directly follow a startElement are merged. The indentLevel remains unchanged (+ 1 - 1)
prettyPrint(String) ->
  PrintFun =
    fun(Event, {Acc, IndentLevel, NewNamespaces, Previous, NoIndent} = In) ->
      Spaces = "    ",
      case {Previous, Event} of
        {_, {ignorableWhitespace, _}} ->
          In;
        {_, {endPrefixMapping, _}} ->
          In;
        {undefined, startDocument} ->
          In;
        {undefined, endDocument} ->
          %% can only occur in case of XML like this: <element/>
          lists:flatten(lists:reverse(Acc));
        {undefined, _} ->
          {Acc, IndentLevel, NewNamespaces, Event, false};
        {{startPrefixMapping, Prefix, URI}, _} ->
          {Acc, IndentLevel, [{Prefix, URI} | NewNamespaces], Event, false};
        {{startElement, _, _, _, _} = SE, {endElement, _, _, _}} ->
          Indent = string:copies(Spaces, IndentLevel),
          TagContent = printStartTagContent(NewNamespaces, SE),
          {[[Indent, $<, TagContent, "/>\n"] | Acc], IndentLevel, [], undefined, false};
        {{startElement, _, _, _, _} = SE, {characters, _}} ->
          Indent = string:copies(Spaces, IndentLevel),
          TagContent = printStartTagContent(NewNamespaces, SE),
          {[[Indent, $<, TagContent, ">"] | Acc], IndentLevel + 1, [], Event, false};
        {{startElement, _, _, _, _} = SE, {startElement, _, _, _, _}} ->
          Indent = string:copies(Spaces, IndentLevel),
          TagContent = printStartTagContent(NewNamespaces, SE),
          {[[Indent, $<, TagContent, ">\n"] | Acc], IndentLevel + 1, [], Event, false};
        {{startElement, _, _, _, _} = SE, {startPrefixMapping, _, _}} ->
          %% Will always be followed by another startElement, so treat like
          %% the clause above
          Indent = string:copies(Spaces, IndentLevel),
          TagContent = printStartTagContent(NewNamespaces, SE),
          {[[Indent, $<, TagContent, ">\n"] | Acc], IndentLevel + 1, [], Event, false};
        {{characters, Characters}, {startElement, _, _, _, _}} ->
          %% mixed scenario, print characters and newline
          {[[xmlString(Characters), $\n] | Acc], IndentLevel, [], Event, false};
        {{characters, Characters}, {endElement, _, _, _}} ->
          %% print characters, no newline
          {[xmlString(Characters) | Acc], IndentLevel, [], Event, true};
        {{characters, Characters}, {characters, _}} ->
          %% value split in 2 (perhaps because of CDATA)
          %% print 1st bit, no newline
          {[xmlString(Characters) | Acc], IndentLevel, [], Event, true};
        {{endElement, _Uri, LocalName, Prefix}, endDocument} ->
          Acc2 = [["</", printPf(Prefix), LocalName, $>] | Acc],
          lists:flatten(lists:reverse(Acc2));
        {{endElement, _Uri, LocalName, Prefix}, _}  ->
          %% print end tag, add new line, decrease indent
          NewIndentLevel = IndentLevel - 1,
          Indent = if NoIndent == true -> []; true -> string:copies(Spaces, NewIndentLevel) end,
          {[[Indent, "</", printPf(Prefix), LocalName, ">\n"] | Acc], NewIndentLevel, [], Event, false};
        {_, _Ignored} ->
          %io:format("ignoring: ~p~n", [_Ignored]),
          %% ignore
          In
      end
    end,
    {ok, R, _} = erlsom:parse_sax(String, {"", 0, [], undefined, false}, PrintFun),
    R.

printStartTagContent(NewNamespaces, {startElement, _, LocalName, Prefix, Attributes}) ->
  Namespaces = printNamespaces(NewNamespaces),
  % silly but the sax parser delivers them in the wrong order.
  AttributeStr = printAttributes(lists:reverse(Attributes)),
  [printPf(Prefix), LocalName, Namespaces, AttributeStr].

printAttributes(Atts) ->
  [[?SPACE, printPf(Prefix), Local, "=\"", xmlString(Value), $\"] ||
   #attribute{localName= Local, prefix = Prefix, value = Value} <- Atts].

printNamespaces(Ns) ->
  [[" xmlns", if Prefix == [] -> []; true -> [$:, Prefix] end, "=\"", URI, $"] ||
   {Prefix, URI} <- Ns].

printPf([]) ->
  [];
printPf(Prefix) ->
  [Prefix, $:].

%% mapping through escapeChar/1 accounts for a 85% time overhead and more than doubles memory consumption.
%% erlsom_ucs:from_utf8/1 is even worse with 280% time overhead and memory consumption of 2 words per character (16x binary size on 64-bit).
xmlString(String) when is_list(String) ->
  lists:map(fun(Char) -> escapeChar(Char) end, String);
xmlString(String) when is_binary(String) ->
  % check plus escape is 3% slower but check and skipping escape is 35x faster
  % binary:match/2 is 10x faster than a naive recursive check
  case binary:match(String, [<<$&>>, <<$">>, <<$<>>]) of
    nomatch -> String;
    _ -> escapeBinary(String, <<>>)
  end.

convert_integer({_, Subtype}, Value) ->
  check_int(Subtype, list_to_integer(strip(Value))).

check_int(nonNegativeInteger, V)
  when V >= 0 -> V;
check_int(negativeInteger,V)
  when V < 0 -> V;
check_int(positiveInteger,V)
  when V > 0 -> V;
check_int(unsignedLong,V)
  when V >= 0, V =< 18446744073709551615 -> V;
check_int(unsignedInt,V)
  when V >= 0, V =< 4294967295 -> V;
check_int(unsignedShort,V)
  when V >= 0, V =< 65535 -> V;
check_int(unsignedByte,V)
  when V >= 0, V =< 255 -> V;
check_int(unsignedLong,V)
  when V >= 0, V =< 18446744073709551615 -> V;
check_int(nonPositiveInteger,V)
  when V =< 0 -> V;
check_int(negativeInteger,V)
  when V < 0 -> V;
check_int(long,V)
  when V >= -9223372036854775807, V =< 9223372036854775808-> V;
check_int(int,V)
  when V >= -2147483648, V =< 2147483647 -> V;
check_int(short,V)
  when V >= -32768, V =< 32767 -> V;
check_int(byte,V)
  when V >= -128, V =< 255 -> V.

escapeChar($&) -> "&amp;";
escapeChar($") -> "&quot;";
escapeChar($<) -> "&lt;";
escapeChar(Char) -> Char.

escapeBinary(<<>>, Acc) -> Acc;
escapeBinary(<<$&,   Rest/binary>>, Acc) -> escapeBinary(Rest, <<Acc/binary, "&amp;">>);
escapeBinary(<<$",   Rest/binary>>, Acc) -> escapeBinary(Rest, <<Acc/binary, "&quot;">>);
escapeBinary(<<$<,   Rest/binary>>, Acc) -> escapeBinary(Rest, <<Acc/binary, "&lt;">>);
escapeBinary(<<Char, Rest/binary>>, Acc) -> escapeBinary(Rest, <<Acc/binary, Char>>).

%% remove all whitespace at the beginning and the end.
strip(String) ->
  strip_right(strip_left(String)).

strip_left([Sc|S]) when ?is_whitespace(Sc) ->
  strip_left(S);
strip_left([_|_] = S) -> lists:reverse(S);
strip_left([]) -> [].

%% Note that this assumes that the string is in reverse order.
strip_right([Sc|S]) when ?is_whitespace(Sc) ->
  strip_right(S);
strip_right([_|_] = S) -> lists:reverse(S);
strip_right([]) -> [].
