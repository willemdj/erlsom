%% translates an erlang type specification to an xsd.


%% The set of type specifications that can be translated is limited 

%% The spec consists of record definitions only.
%% Only integer() and string() can be used as basic types.
%% Lists and unions can be used to structure things (no tuples).
%% All fields will be optional, except if you provide a default value (this is 
%% conform the meaning of the type specs). This is often not what you 
%% want in the XSD. It is easy to fix this in the resulting XSD.

%% 'elements' will be created for all types. You can change this behaviour by 
%% explicitly limiting for which types elements must be created by using
%% a module attribute "-erlsom_xsd_elements([Name])." (It is recommended 
%% to do this, since it will result in better type checking and 
%% a cleaner XSD).

%% a namespace can be specified using a command line option, or using 
%% a special attribute in the file.

%% It is possible to indicate which fields of a record have to be implemented 
%% as attributes by putting a module attribute "-erlsom_xsd_attributes([Name]).", where
%% Name is of the form Record.Field. Attributes have to be declared in this way 
%% before the record in which they are used.
%% Alternativily, the fields can be given a name that starts with '@': '@attribute'.
%% NOTE: only the first (couple of) elements of the record can be 
%% declared as attributes, since Erlsom will always put the attributes first.
%% 

-module(erlsom_type2xsd).

-export([test/0, test/1, type_to_xsd/2]).

-include("erlsom_compile.hrl"). %% the records for XSD elements
-include("erlsom.hrl"). %% qname{} and ns{}

%% testing bits

testString() -> 
  {ok, Binary} = file:read_file("test_hrl.hrl"),
  binary_to_list(Binary).

test() ->
  test([]).

test(_Options) ->
  XsdFile = "test_hrl.xsd",
  type_to_xsd(testString(), XsdFile),
  {ok,Model} = erlsom:compile_xsd_file(XsdFile, [{any_attributes, false}]),
  {ok, Struct, _} = erlsom:scan_file("test_hrl.xml", Model),
  Struct.

%% end of testing bits

-record(state, 
  {elements  = []  %% accumulates the top level elements
  ,types     = []  %% accumulates the types
  ,atts      = []  %% holds the list of elements that must be treated as
                   %% attributes
  ,els       = []  %% the list of 'top level' elements. If empty, all types
                   %% will be made available as elements
  ,ns      %% holds the namespace ({Namespace, Prefix}).
  }).

type_to_xsd(String, XsdFile) ->
  type_to_xsd(String, XsdFile, []).

type_to_xsd(String, XsdFile, Options) ->
  {ok, Tokens, _} = erl_scan:string(String),
  Forms = splitForms(Tokens),
  ParsedForms = [erl_parse:parse_form(Form) || Form <- Forms],
  io:format("parsed: ~p~n", [ParsedForms]),
  Tns = proplists:get_value('target_namespace', Options), 
  #state{elements = Elements, types = Types, ns = Tns2} = translateForms(ParsedForms, #state{ns = Tns}),
  %% write the XSD
  Schema = #schemaType{elements = Elements ++ Types,
    targetNamespace = getTns(Tns2),
    elementFormDefault= "qualified", attributeFormDefault = "unqualified"},
  io:format("Schema: ~p~n", [Schema]),
  writeXsd(Schema, XsdFile).

getTns({Value, _Prefix}) -> Value.

translateForms([], State) ->
  State;
translateForms([Form | T], S) ->
  %% io:format("form: ~p~n", [Form]),
  translateForms(T, translateForm(Form, S)).

%% returns State
translateForm({ok, Form}, State) ->
  translate(Form, State).

translate({attribute, _, record, {Name, Fields}}, 
          State = #state{elements = Els, types = Types, els = ExportEls}) ->
  %% return an element and a type
  ElementName = atom_to_list(Name),
  NewEls = case exportElement(ElementName, ExportEls) of
    true -> 
      [#globalElementType{name = ElementName, type=qname(ElementName)} | Els];
    false ->
      Els
  end,
  {Elements, Attributes} = translateFields(Fields, ElementName, State),
  Model = #sequenceType{elements = Elements},
  Type = #globalComplexTypeType{name = ElementName, attributes = Attributes, model = Model},
  State#state{elements = NewEls, types = [Type | Types]};

translate({attribute, _, erlsom_xsd_elements, Els}, State = #state{els = ElsAcc}) ->
  State#state{els = Els ++ ElsAcc};
%% state.ns holds the namespace ({Namespace, Prefix}).
translate({attribute, _, erlsom_xsd_namespace, {Ns, Pf}}, State) ->
  State#state{ns = {Ns, Pf}};
translate({attribute, _, erlsom_xsd_namespace, Ns}, State) ->
  State#state{ns = {Ns, undefined}};
translate({attribute, _, erlsom_xsd_attributes, Atts}, State = #state{atts = AttsAcc}) ->
  State#state{atts = Atts ++ AttsAcc}.

translateFields(Fields, ElementName, State) ->
  translateFields(Fields, [], [], ElementName, State).

translateFields([], Els, Atts, _ElementName, _State) ->
  {lists:reverse(Els), lists:reverse(Atts)};
translateFields([{typed_record_field, Name, Type} | Tail], Els, Atts, ElementName, State) ->
  {FieldName, MarkedAsAttr} = translateName(Name),
  case isAttribute(FieldName, State#state.atts, ElementName) or MarkedAsAttr of
    true ->
      translateFields(Tail, Els, [translateAttribute(FieldName, Type) | Atts], ElementName, State);
    false ->
      translateFields(Tail, [translateElement(FieldName, Type, State) | Els], Atts, ElementName, State)
  end.

isAttribute(FieldName, Atts, ElementName) ->
  %% Atts is a list of strings "[Element.Field"]
  AttName = ElementName ++ "." ++ FieldName, 
  lists:member(AttName, Atts).

translateElement(FieldName, Type, _State) ->
  {TranslatedType, MinOccurs, MaxOccurs} = translateType(Type),
  case TranslatedType of
    #choiceType{} -> 
      TranslatedType#choiceType{minOccurs = MinOccurs, maxOccurs = MaxOccurs};
    _ ->
      #localElementType{name = FieldName, type = TranslatedType, minOccurs = MinOccurs, maxOccurs = MaxOccurs}
  end.

translateAttribute(Field, Type) ->
  %% TODO: a check on the validity of attribute types
  {TranslatedType, MinOccurs, MaxOccurs} = translateType(Type),
  %% TODO: attributes can be optional
  #localAttributeType{name = Field, type = TranslatedType}.

-spec translateName(Record :: term()) -> {Name :: string(), IsAttribute :: boolean()}.
%% If Name starts with @, IsAttribute = true and @ is stripped of.
translateName({record_field, LineNo, Name, _Default}) ->
  translateName({record_field, LineNo, Name});
translateName({record_field, _, {atom, _, Name}}) ->
  case atom_to_list(Name) of
    [$@ | T] ->
      {T, true};
    Other ->
      {Other, false}
  end.


%% returns {TranslatedType, MinOccurs, MaxOccurs}
%% -record(qname, {uri, localPart, prefix, mappedPrefix}).
%% if the type is a union with 'undefined', the field is optional.
%% Type can be a union, a list, a simple type, ...?
%% The most complicated case is a union, so lets build a list of alternatives.
%% If one of the alternatives = "undefined", we can discard that, and make the
%% entire type optional.
%% If we still have more than 1 alternative left, it is a choice.
translateType({type, _, union, Alternatives}) ->
  FilterUndefined = fun({atom, _, undefined}) -> true;
                       (_) -> false
                    end, 
  FilterDefined = fun(X) -> not(FilterUndefined(X)) end,
  %% look for 'undefined' (and remove it)
  Optional = lists:any(FilterUndefined, Alternatives),
  Alternatives2 = lists:filter(FilterDefined, Alternatives),
  %% now it can either be a single simple type, or a real choice between 2 or more record types
  MinOccurs = case Optional of
    true -> "0";
    _ -> undefined
  end,
  case Alternatives2 of
    [{type, _, SimpleType, _} = TheType] when SimpleType == integer; SimpleType == boolean;
                                              SimpleType == string; SimpleType == record -> %% not really a choice
      {Type, _, MaxOccurs} = translateType(TheType),
      {Type,  MinOccurs, MaxOccurs};
    [{type, _, list, [Element]}] -> %% not really a choice
      {Type, _, _} = translateType(Element),
      {Type,  MinOccurs, "unbounded"};
    _ ->
      TranslatedAlternatives = [translateAlternative(Alternative) || Alternative <- Alternatives2],
      {#choiceType{alternatives = TranslatedAlternatives}, MinOccurs, undefined}
  end;

translateType({type, _, list, [Element]}) ->
  TranslatedElement = translateType(Element),
  {TranslatedElement, "0", "unbounded"};

translateType({type, _, record, [{atom, _, RecordType}]}) ->
  {#qname{localPart = atom_to_list(RecordType)},
    undefined, undefined};

translateType({atom, _, undefined}) ->
  undefined;
translateType({type, _, integer, []}) ->
  {#qname{localPart = "integer", uri = "http://www.w3.org/2001/XMLSchema"},
    undefined, undefined};
translateType({type, _, boolean, []}) ->
  {#qname{localPart = "boolean", uri = "http://www.w3.org/2001/XMLSchema"},
    undefined, undefined};
translateType({type, _, string, []}) ->
  {#qname{localPart = "string", uri = "http://www.w3.org/2001/XMLSchema"},
    undefined, undefined}.

%% alternatives have to be references to records (or lists of those).
translateAlternative({type, _, record, [{atom, _, RecordName}]}) ->
  #localElementType{name = atom_to_list(RecordName), type = #qname{localPart = atom_to_list(RecordName)}}.


splitForms(Tokens) ->
  splitForms(Tokens, [], []).
splitForms([{dot, Line}  | Tail], TokenAcc, FormAcc) ->
  splitForms(Tail, [], [lists:reverse([{dot, Line} | TokenAcc]) | FormAcc]);
splitForms([], [], FormAcc) ->
  lists:reverse(FormAcc);
splitForms([Token | Tail], TokenAcc, FormAcc) ->
  splitForms(Tail, [Token | TokenAcc], FormAcc).

writeXsd(Schema, File) ->
  %% get the model
  Model = erlsom_parseXsd:xsdModel(),
  %% create the Xsd
  %% %% TODO: attributes can be optional
  {ok, R} = erlsom:write(Schema, Model),
  PP = erlsom_lib:prettyPrint(R),
  %% io:format("pretty printed: ~p~n", [PP]),
  file:write_file(File, PP).
  
%% if no elements are declared explitly, all will be part of the XSD.
exportElement(_Element, []) ->
  true;
exportElement(Element, List) ->
  lists:member(Element, List).


%% -record(qname, {uri, localPart, prefix, mappedPrefix}).
qname(LocalPart) ->
  #qname{localPart = LocalPart, uri = ""}.
