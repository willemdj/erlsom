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
%%% translates a data structure of a pre-defined form to an XML document.
%%% ====================================================================

%%% This is the companion of erlsom_parse, which performs the inverse operation.
%%% Both modules use the same 'model' that describes the translation, see the
%%% introduction to erlsom_parse for the definition of this model.

-module(erlsom_write).
-export([write/2,
         write/3]).

-include("erlsom_parse.hrl").
-include("erlsom.hrl").

%% debug(Text) ->
  %% io:format("write: ~p\n", [Text]).

%% Returns the XML document. {ok, Document}
write(Struct, Model) ->
  write(Struct, Model, []).

write(Struct, Model = #model{tps = Types}, Options) ->

  %% start with _document type.
  case lists:keysearch('_document', 2, Types) of
    {value, #type{els = [Head | _Tail], mxd = Mixed}} ->
      CurrentValue = Struct,
      ResultWithThisElement =
        processElementValues([CurrentValue], Head,
                             [], 0, Model, {[], 0}, Mixed),
      %% debug(ResultWithThisElement);
      case proplists:get_value(output, Options, list) of
        list ->
          {ok, unicode:characters_to_list(ResultWithThisElement)};
        chardata ->
          {ok, ResultWithThisElement};
        binary ->
          {ok, unicode:characters_to_binary(ResultWithThisElement)}
      end;
    _Else ->
      {error, "Model should have _document type"}
  end.

struct2xml(_Struct, [], ResultSoFar, _Model, _Namespaces, _Mixed) ->
  lists:reverse(ResultSoFar);

%% Struct = {RecordType, value, ...}
%% Processes whatever is INSIDE the tags (= the values of the struct), one by one, from the first
struct2xml(Struct,
           _StructModel = [ModelForThisElement = #el{alts = Alternatives, mn = Min, mx = Max, nr = SequenceNr,
                           nillable = Nillable} | NextElements],
           ResultSoFar, Model = #model{nss = Namespaces}, DeclaredNamespaces,
           Mixed) ->

  %% Which alternative has been selected follows from the value of this element
  CurrentValue = element(SequenceNr, Struct),
  %% value = tuple => subtype
  %% value = list of chars (integers) => text
  %% value = list (not string) of:
  %%              tuples => subtypes, element has maxOccurs > 1 OR alternative has maxOccurs > 1
  %%              strings =>
  %%              lists => element has maxOccurs > 1 AND alternative has maxOccurs > 1
  %% value == undefined -> no value provided
  %% etc.

  %% debug(CurrentValue),

  if
    (Max == 1) and (Mixed /= true) ->
      case CurrentValue of
        undefined ->
          if
            Min > 0  ->
              Tags = [Alt#alt.tag || Alt <- Alternatives],
              Error = lists:flatten(io_lib:format("No value provided for non-optional element ~p~n", [Tags])),
              throw({error, Error});
            true -> ok
          end,
          ResultForThisElement = [];
        nil ->
          if
            Nillable /= true -> throw({error, "nil value provided for non-nillable element"});
            true -> ok
          end,
          ResultForThisElement = printNilValue(Alternatives, Namespaces, DeclaredNamespaces);
        {nil, AttrValues} ->
          if
            Nillable /= true -> throw({error, "nil value provided for non-nillable element"});
            true -> ok
          end,
          ResultForThisElement = printNilValue(Alternatives, AttrValues, Model, Namespaces, DeclaredNamespaces);
        [V1 | _] ->
          case V1 of
            _ when is_integer(V1) -> %% CurrentValue is a string
              ResultForThisElement = printValue(CurrentValue, Alternatives, Namespaces, DeclaredNamespaces, Mixed);
            _ when is_tuple(V1) ->
              %% debug("alternative with MaxOccurs > 1"),
              ResultForThisElement = processAlternatives(CurrentValue, Alternatives, Model, DeclaredNamespaces,
                                                         Mixed)
          end;
        #qname{} ->
          ResultForThisElement = printValue(CurrentValue, Alternatives, Namespaces, DeclaredNamespaces, Mixed);
        _ when is_tuple(CurrentValue) ->
          %% debug("subtype"),
          ResultForThisElement =
                 processElementValues([CurrentValue], ModelForThisElement, [], 0, Model, DeclaredNamespaces, Mixed);
        _ when is_binary(CurrentValue) ->
          ResultForThisElement = printValue(CurrentValue, Alternatives, Namespaces, DeclaredNamespaces, Mixed);
        _ ->
          %% debug("simple type"),
          ResultForThisElement = printValue(CurrentValue, Alternatives, Namespaces, DeclaredNamespaces, Mixed)
      end;
    true -> %% CurrentValue is a list, because Element has maxOccurs > 1.
      if
        is_list(CurrentValue); CurrentValue == undefined -> true;
        true -> throw({error, "value has to be a list"})
      end,
      if
        CurrentValue == undefined ->
          ResultForThisElement = [];
        true ->
          ResultForThisElement = processElementValues(CurrentValue, ModelForThisElement, [], 0, Model,
                                                      DeclaredNamespaces, Mixed)
      end
  end,

  %% process remaining elements
  struct2xml(Struct, NextElements, [ResultForThisElement | ResultSoFar], Model, DeclaredNamespaces, Mixed).


processElementValues([],
                     _ModelForThisElement = #el{mn = Min},
                     ResultSoFar, Counter, _Model, _DeclaredNamespaces,
                     _Mixed) ->
  if
    Counter < Min ->
      throw({error, "Not enough values provided"});
    true ->
      lists:reverse(ResultSoFar)
  end;

%% ElementValues can be:
%%
%% FirstElement can be:
%% - a tuple
%%    - for a value
%% - a string (list)
%%    - for a value
%% - a list of tuples:
%%    - for a sequence (or all, no need to distinguish) where the element has more values
%%    - for a choice where the selected alternative has maxOccurs > 1
%% - a list of lists
%%    - for a choice with maxOccurs > 1 and an alternative with maxOccurs > 1
%%    - it could in theory be a list of lists of lists (etc.)? But not now, since choice in choice is
%%      not supported.
%% - nil
%%    - for a nillable element without attributes - even though it is not clear how useful this is
%% - {nil, Record}
%%    - for a nillable element with attributes
processElementValues([V1 | NextValues],
               ModelForThisElement = #el{alts = Alternatives, mx = Max, nillable=Nillable},
               ResultSoFar, Counter, Model = #model{nss = Namespaces}, DeclaredNamespaces, Mixed) ->

  %% debug("procesElementValues, counter = " ++ integer_to_list(Counter)),
  {Case, IncreaseCounter} =
    case V1 of
      [] -> %% "", string of 0 characters
        {listOfStrings, 1};
      _ when is_list(V1) ->
        V11 = hd(V1),
        case V11 of
          #qname{} ->
            throw({error, "wrong type in value"});
          _ when is_tuple(V11) ->
            {listOfTuples, 1};
          _ when is_integer(V11) ->
            if
              Mixed == true ->
                {mixed, 0};
              true ->
                %% debug("element w. MaxOccurs > 1; 1st value is a string"),
                {listOfStrings, 1}
              end;
          _ ->
            throw({error, "wrong type in value"})
        end;
      _ when is_binary(V1) ->
         if
           Mixed == true ->
             {mixed, 0};
           true ->
             %% debug("element w. MaxOccurs > 1; 1st value is a string"),
             {listOfStrings, 1}
         end;
      #qname{} ->
        %% debug("element w. MaxOccurs > 1, (1st value is a qname)."),
        {qname, 1};
      {nil, _Record}  ->
        if
          Nillable /= true -> throw({error, "nil value provided for non-nillable element"});
          true -> {V1, 1} %% {{nil, Values}, 1} -- a bit tricky
        end;
      _ when is_tuple(V1) ->
        %% debug("element w. MaxOccurs > 1, (1st value is a subtype)"),
        {tuple, 1};
      nil ->
        if
          Nillable /= true -> throw({error, "nil value provided for non-nillable element"});
          true -> {nil, 1}
        end;
      _ ->
        %% debug("element w. MaxOccurs > 1, (1st value is a simple type)"),
        {simpleType, 1}
    end,
  if
    Counter + IncreaseCounter > Max ->    %% if Max == unbound the result of the test is false!
      throw({error, "Too many values provided"});
    true -> true
  end,
  ResultForThisElement =
    case Case of
      listOfTuples ->
        processAlternatives(V1, Alternatives, Model, DeclaredNamespaces, Mixed);
      mixed ->
        erlsom_lib:xmlString(V1);
      listOfStrings ->
        printValue(V1, Alternatives, Namespaces, DeclaredNamespaces, Mixed);
      qname ->
        printValue(V1, Alternatives, Namespaces, DeclaredNamespaces, Mixed);
      tuple ->
        processSubType(V1, Alternatives, Model, DeclaredNamespaces, Mixed);
      simpleType ->
        printValue(V1, Alternatives, Namespaces, DeclaredNamespaces, Mixed);
      nil ->
        printNilValue(Alternatives, Namespaces, DeclaredNamespaces);
      {nil, Record} ->
        printNilValue(Alternatives, Record, Model, Namespaces, DeclaredNamespaces)
    end,

    processElementValues(NextValues, ModelForThisElement, [ResultForThisElement | ResultSoFar], Counter + IncreaseCounter,
                       Model, DeclaredNamespaces, Mixed).

%% returns a string that represents the value
processSubType(Value, Alternatives, Model = #model{tps = Types}, DeclaredNamespaces,
               Mixed) ->
  %% RecordType can be an instantiated abstract type
  RecordType = element(1, Value),
  {Alternative, Abstract} = findAlternative(RecordType, Alternatives, Model),
  TypeRecord = findType(RecordType, Types),
  processAlternativeValue(Value, 1, Alternative, TypeRecord, Model, DeclaredNamespaces, Abstract, Mixed).

processAlternatives(Values = [Value | _], Alternatives, Model = #model{tps = Types},  DeclaredNamespaces,
                    Mixed) ->
  %% See which alternative this is
  RecordType = element(1, Value),
  {Alternative, Abstract} = findAlternative(RecordType, Alternatives, Model),
  TypeRecord = findType(RecordType, Types),
  processAlternativeValues(Values, 0, Alternative, TypeRecord, Model, DeclaredNamespaces, Abstract, [], Mixed).

processAlternativeValues([], Count, #alt{mn = Min}, _Type, _Model, _Ns, _Abstract, Acc, _Mixed) ->
  if
    Count < Min -> throw({error, "not enough values"});
    true -> lists:reverse(Acc)
  end;

processAlternativeValues([V1 | Tail], Count, Alternative, Type, Model, Ns, Abstract, Acc, Mixed) ->
  processAlternativeValues(Tail, Count + 1, Alternative, Type, Model, Ns, Abstract,
      [processAlternativeValue(V1, Count, Alternative, Type, Model, Ns, Abstract, Mixed) | Acc], Mixed).

processAlternativeValue(Value, Count,
                        #alt{tag = Tag, rl = RealElement, mx = MaxAlt},
                        #type{els = Elements, atts = Attributes, typeName = Name, mxd = MixedChild},
                        Model = #model{nss = Namespaces, any_attribs = AnyAtts},
                        DeclaredNamespaces,
                        Abstract,
                        MixedParent) ->

  Mixed =
    case MixedParent of
      true ->
        case RealElement of
          true -> MixedChild;
          simple -> MixedChild;
          _ -> MixedParent %% 'non-real' elements (groups) inherit this property
        end;
      _ -> MixedChild
    end,

  if
   Count > MaxAlt -> throw({error, "too many values"});
   true -> true
  end,

  TagAsText = atom_to_list(Tag),
  if
    RealElement ->
      %% process the attributes
      {AttributesString, NewDeclaredNamespaces} = processAttributes(Value, [], Attributes, Namespaces, DeclaredNamespaces),
      %% process anyAttributes
      %% for now we don't check whether 'anyAttributes' are allowed!
      {AnyAttributesString, DeclaredNamespaces2} =
        case AnyAtts of
          true ->
            processAnyAttributes(element(2, Value), [], Namespaces, NewDeclaredNamespaces);
          _ ->
            {"", NewDeclaredNamespaces}
        end,
      {AnyAttrPlusXsiTypeString, DeclaredNamespaces3} =
        case Abstract of
          false -> {AnyAttributesString, DeclaredNamespaces2};
          _ ->
            NameAsText = atom_to_list(Name),
            {NamespacesStringAbs, DeclaredNamespacesAbs, _ExtraPrefixAbs} = processNamespaces(NameAsText, Namespaces, DeclaredNamespaces2),
            XsiType = [" xsi:type=\"", atom_to_list(Name), "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""],
            {[AnyAttributesString, XsiType, NamespacesStringAbs], DeclaredNamespacesAbs}
     end,

      %% deal with namespaces (that is, see if they have to be declared here)
      {NamespacesString, NewDeclaredNamespaces4, Extra_prefix} = processNamespaces(TagAsText, Namespaces, DeclaredNamespaces3),
      ResultForThisElement = struct2xml(Value, Elements, [], Model, NewDeclaredNamespaces4, Mixed),
      AllAttrs = [NamespacesString, AttributesString, AnyAttrPlusXsiTypeString],
      printTag([Extra_prefix, TagAsText], AllAttrs, ResultForThisElement);
    true ->
      struct2xml(Value, Elements, [], Model, DeclaredNamespaces, Mixed)
  end.


printTag(TagAsText, AllAttrs, []) ->
  %% Short tag
  [$<, TagAsText, AllAttrs, "/>"];
printTag(TagAsText, AllAttrs, Content) ->
  %% Open, close tags
  [$<, TagAsText, AllAttrs, ">", Content, "</", TagAsText, $>].


findType(TypeName, Types) ->
  case lists:keysearch(TypeName, #type.nm, Types) of
    {value, Type} -> Type;
    _ -> throw({error, "Something wrong with the Model"})
  end.

findAlternative(RecordType, Alternatives, Model) ->
  findAlternative(RecordType, Alternatives, Model, false).

findAlternative(RecordType, Alternatives, #model{th = TypeHierarchy} = Model, Abstract) ->
  case lists:keysearch(RecordType, #alt.tp, Alternatives) of
    {value, Alternative} -> {Alternative, Abstract};
    _ ->
        %% see whether this is an '#any' type
        case Alternatives of
          [#alt{tag='#any', anyInfo = #anyInfo{ns = AltNs}}] when AltNs /= "##other" ->
            AnyAlternatives = Alternatives ++ erlsom_lib:documentAlternatives(Model),
            findAlternative(RecordType, AnyAlternatives, Model, Abstract);
          _ ->
            %% see whether there is an "anyType" alternative
            case lists:keysearch('#ANY', #alt.tp, Alternatives) of
              {value, AnyAlternative} ->
                %% return this alternative, but with the type set to the actual type
                {AnyAlternative#alt{tp = RecordType}, Abstract};
            _ ->
            %% see whether an ancestor in the type hierarchy is among the alternatives
            case erlsom_lib:getAncestor(RecordType, TypeHierarchy) of
              {value, Ancestor} ->
                findAlternative(Ancestor, Alternatives, Model, true);
              _ ->
                throw({error, "Struct doesn't match model: recordtype not expected: " ++ atom_to_list(RecordType)})
            end
        end
        end
  end.


%% Attribute is a tuple {Name, SequenceNr, Optional, Type}
processAttributes(_Struct, ResultSoFar, [], _Namespaces, DeclaredNamespaces) ->
  {ResultSoFar, DeclaredNamespaces};

processAttributes(Struct, ResultSoFar, [#att{nm = Name,
                                             nr = SequenceNr,
                                             opt = Optional,
                                             tp = Type} | Rest], Namespaces, DeclaredNamespaces) ->
  NameAsString = atom_to_list(Name),
  {NamespacesString, NewDeclaredNamespaces, _Extra_prefix} = processNamespaces(NameAsString, Namespaces, DeclaredNamespaces),
  AttributeValue = element(SequenceNr, Struct),
  case AttributeValue of
    undefined ->
      if
        Optional ->
          processAttributes(Struct, ResultSoFar, Rest, Namespaces, DeclaredNamespaces);
        true ->
          throw({error, "No value provided for mandatory attribute " ++ atom_to_list(Name)})
      end;
    _Defined ->
      case Type of

        String when String == char; String == ascii ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = erlsom_lib:xmlString(AttributeValue);
        integer ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = try integer_to_list(AttributeValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type in attribute  " ++ atom_to_list(Name) ++ ", expected Integer"})
          end;
        bool ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = case AttributeValue of
                        true -> "true";
                        false -> "false";
                        _ -> throw({error, "Wrong Type in attribute  " ++ atom_to_list(Name) ++ ", expected boolean"})
                      end;
        float ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = try float_to_xml(AttributeValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type in attribute  " ++ atom_to_list(Name) ++ ", expected Float"})
          end;
        atom ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = try atom_to_list(AttributeValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type in attribute  " ++ atom_to_list(Name) ++ ", expected Atom"})
          end;
        qname ->
          {CharValue, NamespacesString2, DeclaredNamespaces2} =
             try writeQnameAttValue(AttributeValue, NamespacesString, Namespaces, NewDeclaredNamespaces)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type in attribute " ++ atom_to_list(Name) ++ ", expected qname, got " ++
                     lists:flatten(io_lib:format("~p",[AttributeValue]))})
          end;
        {integer, Subtype} ->
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = try
                        erlsom_lib:check_int(Subtype, AttributeValue),
                        integer_to_list(AttributeValue)
                      catch
                        _AnyClass:_Any ->
                          throw({error, "Wrong Type in attribute " ++ atom_to_list(Name) ++ ", expected " ++ atom_to_list(Subtype)})
                      end;
        _Else ->
          throw({error, "unknown type in model: " ++lists:flatten(io_lib:format("~p",[Type]))}),
          DeclaredNamespaces2 = NewDeclaredNamespaces,
          NamespacesString2 = NamespacesString,
          CharValue = []
      end,
      %% NamespacesString is "" or " xmlns...".
      ResultWithThisAttribute = [ResultSoFar, NamespacesString2, " ", NameAsString, "=\"", CharValue, "\""],
      processAttributes(Struct, ResultWithThisAttribute, Rest, Namespaces, DeclaredNamespaces2)
  end.


%% returns:
%% {AttributeValue, NamespacesString, NewDeclaredNamespaces}
%% -record(qname, {uri, localPart, prefix, mappedPrefix}).
writeQnameAttValue(#qname{uri = Uri, localPart = LP, mappedPrefix = MP}, NamespacesString, Namespaces,
                   DeclaredNamespaces = {NamespacesList, Counter}) ->
  case Uri of
    None when None == []; None == undefined ->
      {LP, NamespacesString, DeclaredNamespaces};
    _ ->
      %% see whether the namespace has been defined. If not, then this has to be done.
      case lists:keysearch(Uri, 2, NamespacesList) of
        {value, {Prefix, _Uri}} -> %% already declared
          {[Prefix, ":", LP], NamespacesString, DeclaredNamespaces};
        _ ->
          %% see whether a prefix was specified
          case lists:keysearch(Uri, #ns.uri, Namespaces) of
            {value, #ns{prefix = Prefix2}} ->
              {[Prefix2, ":", LP], [NamespacesString, " xmlns:", Prefix2, "=\"", Uri, "\""],
               {[{Prefix2, Uri} | NamespacesList], Counter}};
            _ ->
              {[MP, ":", LP], [NamespacesString, " xmlns:", MP, "=\"", Uri, "\""],
               {[{MP, Uri} | NamespacesList], Counter}}
          end
      end
  end.


processAnyAttributes(undefined, Acc, _Namespaces, DeclaredNamespaces) ->
  {Acc, DeclaredNamespaces};
processAnyAttributes([], Acc, _Namespaces, DeclaredNamespaces) ->
  {Acc, DeclaredNamespaces};
processAnyAttributes([{{Name, Uri}, Value} | Tail], Acc, Namespaces, DeclaredNamespaces) ->
  case Uri of
    [] ->
      processAnyAttributes(Tail, [Acc, " ", Name, "=\"", decodeIfRequired(Value), "\""],
        Namespaces, DeclaredNamespaces);
    _Other ->
      %% the "xsi:nil=true" and xsi:type are not written, because they are inserted in another way.
      case {Name, Uri, Value} of
        {"nil", "http://www.w3.org/2001/XMLSchema-instance", "true"} ->
          processAnyAttributes(Tail, Acc, Namespaces, DeclaredNamespaces);
        {"nil", "http://www.w3.org/2001/XMLSchema-instance", "1"} ->
          processAnyAttributes(Tail, Acc, Namespaces, DeclaredNamespaces);
        {"type", "http://www.w3.org/2001/XMLSchema-instance", _} ->
          processAnyAttributes(Tail, Acc, Namespaces, DeclaredNamespaces);
        _ ->
          %% get prefix +, if relevant, NS declaration text
          {PrefixedName, DeclaredNamespaces2} = processAnyNamespaces(Name, Uri, Namespaces, DeclaredNamespaces),
          processAnyAttributes(Tail, [Acc, " ", PrefixedName, "=\"", decodeIfRequired(Value), "\""],
                               Namespaces, DeclaredNamespaces2)
      end
  end.


%% see if the tag references a namespace. If so, see whether
%% this namespace has been declared.
%%
%% Namespaces is of the form {Prefix, URI}
%% DeclaredNamespaces = [Prefix]
%%
%% returns {NameSpacesString, NewDeclaredNamespaces}, where
%% NamespacesString is the declaration (if required), and
%% NewDeclaredNamespaces is the new list of declared
%% namespaces.
processNamespaces(Tag, Namespaces, DeclaredNamespaces = {NamespacesList, Counter}) ->
  %% look for ':' in the tag
  %% debug(Tag),
  %% debug(DeclaredNamespaces),
  Prefix = case string:tokens(Tag, ":") of
             [Pf, _LName] ->
               Pf;
             [_LName] ->
               undefined;
             _ ->
               throw({error, "Tag " ++ Tag ++ " is not of form [prefix:]localName"})
           end,

  %% IF
  %%     prefix is 'undefined' AND
  %%     namespace for 'undefined' exists AND
  %%     elementFormDefault (efd) for this ns is 'unqualified' AND
  %%     the namespace has not be been declared at a higher level
  %% THEN
  %%     put a prefix anyway - make one up. (for now: use "erlsom").


  %% declaredNamespaces = [{Prefix, Uri}]
  case lists:keysearch(Prefix, 1, NamespacesList) of
    {value, _} -> %% already declared
      {[], DeclaredNamespaces, ""};
    _Else ->
      %% find prefix in Model
      case lists:keysearch(Prefix, #ns.prefix, lists:reverse(Namespaces)) of
        {value, #ns{uri = Uri, efd = qualified}} ->
          Xmlns = case Prefix of
                   undefined -> " xmlns";
             _ -> [" xmlns:", Prefix]
           end,
          {[Xmlns, "=\"", Uri, "\""], {[{Prefix, Uri} | NamespacesList], Counter}, ""};
        {value, #ns{uri = Uri, efd = unqualified}} ->
          case Prefix of
            undefined ->
              Xmlns = " xmlns:erlsom",
              Additional_pf = "erlsom:";
            _ ->
              Xmlns = [" xmlns:", Prefix],
              Additional_pf = ""
        end,
        {[Xmlns, "=\"", Uri, "\""], {[{Prefix, Uri} | NamespacesList], Counter}, Additional_pf};
      _ ->
        case Prefix of
          undefined -> {[], DeclaredNamespaces, ""};
          "xml" -> {[], DeclaredNamespaces, ""};
          _ -> throw({error, "Inconsistency in model: namespace is not declared - " ++ Prefix})
        end
      end
  end.

processAnyNamespaces(Name, Uri, Namespaces, {NamespacesList, Counter} = DeclaredNamespaces ) ->
  case lists:keysearch(Uri, 2, NamespacesList) of
    {value, {Prefix, _}} -> %% already declared
      {[Prefix, ":", Name], DeclaredNamespaces};
    _Else ->
      %% find Uri in Model
      case lists:keysearch(Uri, #ns.uri, Namespaces) of
        {value, #ns{prefix = ModelPrefix}} ->
          ThePrefix = ModelPrefix;
        _Else2 ->
          %% make up a prefix, using counter
          ThePrefix = "pre" ++ integer_to_list(Counter +1)
      end,
      PrefixName = [" xmlns:", ThePrefix, "=\"", Uri, "\" ", ThePrefix, ":", Name],
      {PrefixName,
          {[{ThePrefix, Uri} | NamespacesList], Counter + 1}}
  end.

printValue(CurrentValue, Alternatives, Namespaces,
           DeclaredNamespaces = {NamespacesList, _Counter}, Mixed) ->
  case CurrentValue of
    #qname{localPart = LocalName, prefix = Prefix, uri = Uri} ->
      case lists:keysearch({'#PCDATA', qname}, #alt.tp, Alternatives) of
        {value, #alt{tag = Tag, rl = RealElement}} ->  %% Print Tags if RealElement
          %% see whether this namespace was already declared
          {PrintPrefix, NsDecl} = printPrefix(Uri, Prefix, NamespacesList, Namespaces),
          TextValue = case PrintPrefix of
                        undefined -> erlsom_lib:xmlString(LocalName);
                        _ -> erlsom_lib:xmlString(PrintPrefix ++ ":" ++ LocalName)
                      end,
          printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces, NsDecl);
        _Else ->
          throw({error, "Type of value (qname) does not match model"++lists:flatten(io_lib:format("Value = --> ~p <--]",[CurrentValue]))})
      end;

    _B when is_list(CurrentValue); is_binary(CurrentValue) ->
      if
        Mixed == true ->
          erlsom_lib:xmlString(CurrentValue); %% Note: values for (non-mixed) elements have to be of the
                                   %% form {type, Value} within mixed types
        true ->
          case lists:keysearch({'#PCDATA', char}, #alt.tp, Alternatives) of
            {value, #alt{tag = Tag, rl = RealElement}} ->  %% Print Tags if RealElement
              printElement(erlsom_lib:xmlString(CurrentValue), Tag, RealElement, Namespaces, DeclaredNamespaces);
            _Else ->
              case lists:keysearch('#ANY', #alt.tp, Alternatives) of
                {value, #alt{tag = Tag, rl = true}} ->  %% always a "real" element?
                  printElement(erlsom_lib:xmlString(CurrentValue), Tag, true, Namespaces, DeclaredNamespaces);
                _ ->
              throw({error, "Type of value (list) does not match model"++lists:flatten(io_lib:format("Value = --> ~p <-- Expected ~p",[CurrentValue,Alternatives]))})
          end
          end
      end;

    _C when is_integer(CurrentValue) ->
      %% is an integer also a float?
      case lists:keysearch({'#PCDATA', integer}, #alt.tp, Alternatives) of
        {value, #alt{tag = Tag, rl = RealElement}} ->
          TextValue = try integer_to_list(CurrentValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type"})
          end,
          printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces);
        _Else ->
          try convert_int_subtype(CurrentValue, Alternatives) of
            {ok, #alt{tag = Tag, rl = RealElement}, Converted} ->
              printElement(Converted, Tag, RealElement, Namespaces, DeclaredNamespaces);
            _ ->
              throw({error, "Type of value (integer) does not match model"++
                     lists:flatten(io_lib:format("Value = --> ~p <--]",[CurrentValue]))})
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type"})
          end
      end;

    _D when is_float(CurrentValue);
            CurrentValue =:= 'NaN';
            CurrentValue =:= 'INF';
            CurrentValue =:= '-INF'  ->
      %% is an integer also a float?
      case lists:keysearch({'#PCDATA', float}, #alt.tp, Alternatives) of
        {value, #alt{tag = Tag, rl = RealElement}} ->
          TextValue = try float_to_xml(CurrentValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type"})
          end,
          printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces);
        _Else ->
          throw({error, "Type of value (float) does not match model"++lists:flatten(io_lib:format("Value = --> ~p <--]",[CurrentValue]))})
      end;

    _E when CurrentValue ==  true; CurrentValue == false ->
      case lists:keysearch({'#PCDATA', bool}, #alt.tp, Alternatives) of
        {value, #alt{tag = Tag, rl = RealElement}} ->
          TextValue = try atom_to_list(CurrentValue)
          catch
            _AnyClass:_Any ->
              throw({error, "Wrong Type"})
          end,
          printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces);
        _Else ->
          throw({error, "Type of value (atom) does not match model"++lists:flatten(io_lib:format("Value = --> ~p <--]",[CurrentValue]))})
      end;

    _Else ->
      throw({error, "Type of value not valid for XML structure"++lists:flatten(io_lib:format("Value = --> ~p <--]",[CurrentValue]))})
  end.

%% see if there is an {integer, subtype} alternative,
%% test the value
convert_int_subtype(_Value, []) ->
  error;
convert_int_subtype(Value, [#alt{tp = {'#PCDATA', {integer, Subtype}}} = Alt | _T]) ->
  erlsom_lib:check_int(Subtype, Value),
  Converted = integer_to_list(Value),
  {ok, Alt, Converted};
convert_int_subtype(Value, [_H | T]) ->
  convert_int_subtype(Value, T).

printNilValue([#alt{tag= Tag}], Namespaces, DeclaredNamespaces) ->
  printElement("", Tag, true, Namespaces, DeclaredNamespaces,
    " xsi:nil=\"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"");
printNilValue(_Alternatives, _, _) ->
  throw({error, "Nillable cannot be a choice"}).

printNilValue([#alt{tag=Tag, tp = RecordType}], Value, #model{tps = Types, any_attribs = AnyAtts}, Namespaces, DeclaredNamespaces) ->
  TypeRecord = findType(RecordType, Types),
  Attributes = TypeRecord#type.atts,

  %% process the attributes
  {AttributesString, NewDeclaredNamespaces} = processAttributes(Value, [], Attributes, Namespaces, DeclaredNamespaces),
  %% process anyAttributes
  %% for now we don't check whether 'anyAttributes' are allowed!
  {AnyAttributesString, DeclaredNamespaces2} =
    case AnyAtts of
      true ->
        processAnyAttributes(element(2, Value), [], Namespaces, NewDeclaredNamespaces);
      _ ->
        {"", NewDeclaredNamespaces}
    end,

  %% deal with namespaces (that is, see if they have to be declared here)
  TagAsText = atom_to_list(Tag),
  {NamespacesString, _DeclaredNamespaces3, Extra_prefix} = processNamespaces(TagAsText, Namespaces, DeclaredNamespaces2),
  %% print startTag
  StartTag = [$<, Extra_prefix, TagAsText, NamespacesString, AttributesString, AnyAttributesString,
              " xsi:nil=\"true\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">"],
  %% print end tag
  EndTag = ["</", atom_to_list(Tag), ">"],
  [StartTag, EndTag].

printPrefix(undefined, Prefix, _NamespacesList, _Namespaces) ->
  {Prefix, []};

printPrefix(Uri, Prefix, NamespacesList, Namespaces) ->
  case lists:keysearch(Uri, 2, NamespacesList) of
    {value, {Prefix2, _Uri}} -> %% already declared
      {Prefix2, []};
    _ ->
      %% see whether a prefix was specified
      PrintedPrefix = case lists:keysearch(Uri, #ns.uri, Namespaces) of
                        {value, #ns{prefix = Prefix3}} ->
                          Prefix3;
                        _ ->
                          Prefix
                      end,
      Xmlns = case PrintedPrefix of
                undefined -> " xmlns";
                _ -> [" xmlns:", Prefix]
              end,
      {PrintedPrefix, [Xmlns, "=\"", Uri, "\""]}
  end.

printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces, QnameNs) ->
  if
    RealElement ->
       TagAsText = atom_to_list(Tag),
       %% this function is only used in 'leaves' of the struct, so we don't need to store the
       %% new declared namespaces (since those would apply only to child-elements, of
       %% which there are none)
       {NamespacesString, _, Extra_prefix} = processNamespaces(TagAsText, Namespaces, DeclaredNamespaces),
       [$<, Extra_prefix, TagAsText, NamespacesString, QnameNs, $>,  TextValue, "</", Extra_prefix, TagAsText, $>];
    true ->
       TextValue
  end.

printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces) ->
  printElement(TextValue, Tag, RealElement, Namespaces, DeclaredNamespaces, []).

decodeIfRequired(Text) when is_binary(Text) ->
  erlsom_ucs:decode_utf8(Text);
decodeIfRequired(Text) ->
  Text.

float_to_xml('NaN') -> "NaN";
float_to_xml('INF') -> "INF";
float_to_xml('-INF') -> "-INF";
float_to_xml(Float) ->
  io_lib:format("~e", [Float]).

