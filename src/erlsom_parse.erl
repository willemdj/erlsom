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
%%% A callback function for erlsom_sax that translates an XML document
%%% to an Erlang data structure.
%%% ====================================================================

%% translate XML (that follows a certain schema - grammar) to a
%% predefined structure (a set of linked records).
%%
%% The XSD is translated to a structure (called the 'Model'in the code below)
%% that drives the parser, the parser self is generic.
%%
%% The main function is a callback that will be called by the SAX parser.
%% The parameters are:
%% - Event
%% - State
%% as defined by the SAX parser, where:
%% - Event is an event in accordance with the SAX convention, see the SAX
%%   documentation.
%% - State is a tuple {CurrentState, ResultSoFar, Model}
%%
%% The Model can be compared to a (hierarchical - if that is the word) state
%% machine. The sax parser calls the call-back function when it has parsed a new
%% element; this are the events that drive the state machine. The Model
%% determines what the next state will be, given a combination {CurrentState,
%% Event}. While we are working our way through the state machine, we are at the
%% same time producing a result. At each moment, ResultSoFar contains the
%% elements of the final result that have been produced so far. The process ends
%% either when a valid end state is reached, in which case a complete structure
%% is produced as the result, or when an element is parsed that doesn't fit the
%% model. In that case an exception is raised.
%%
%% The events are taken from an input stream. With a given state transition, the
%% first (current) event in the stream may be 'consumed' (taken from the
%% stream), or it may be left in the stream. If it is left in the stream, this
%% event will be the first event that is considered in the new state.  (In other
%% words: the state machine has a look-ahead of 1).
%%
%% The Model is a tuple {Types, NamespaceMapping}.
%% Types is a list of Type, these Types correspond to the types (or
%% elements) that can occur in the XML document.
%% in record form: #model{tps, ns}
%%
%% Each Type is of the form
%% {TypeName, TypeType, Elements, Attributes, NrOfElements}.
%% - TypeName is an atom.
%% - TypeType is currently always 'sequence', meaning that the elements have to
%%   be received in the specified order. In the future it might be considered to
%%   add something like 'bag' or 'set'.
%% - Elements is a list of Element. These Elements correspond to the elements
%%   that together form the Type.
%% - Attributes is a list of Attribute. Each Attribute describes an attribute
%%   that may occur as part of the Type.
%% - NrOfElements is an integer that indicates the number of elements in the Type.
%% In record form: #type{nm, tp, els, atts, nr}
%%
%% There is 1 special Type; this corresponds to the root of the XML document. It
%% has a standard TypeName: '_document'.
%%
%% Each Element is of the form {Alternatives, SequenceNumber}.
%%
%% - Alternatives is a list of Aternative. Quite often there will be only 1
%%   alternative. If there are several possibilities that may occur at this
%%   position in the type (a 'choice', in XSD terminology), they will be listed
%%   here.
%% - SequenceNumber is the position of the Element in the Elements list. This
%%   value is redundant, it is only there because I thought it would be easier,
%%   and maybe more performant.
%% In record form: #el{alts, nr}
%%
%% Note: anyAttr field has been added to #el{}. If its value =/= undefined,
%% 'anyAttribute' was specified in the XSD and should be taken into account.
%%
%% Each Alternative is of the form {Tag, TypeReference, MinOccurs, MaxOccurs,
%% RealElement}.
%% - Tag is generally the tag of the element. In case of a 'choice', the
%%   combination of the Tag and the next event determines which alternative is
%%   selected. Tag can also be '#text', in which case we expect a 'character'
%%   event.
%% - TypeReference is either a reference to a Type (a TypeName), or a tuple
%%   {#PCDATA, PredefinedType}. This corresponds roughly to the next state
%%   of the state machine. In otherwords: "If we receive a start tag A, look i
%%   for the Alternative with Tag=A, and start parsing a type as indicated by
%%   TypeReference".
%% - MinOccurs is the number of times this Element may occur. The value 0 means
%%   that the Element is optional.
%% - MaxOccurs is the maximum number of times the Element may occur. The value
%%   'unbound' indicates that there is no limit.
%% - RealElement can be 'true' or 'false'. If it is 'false', then the current
%%   event will be left in the input stream. If it is 'true', the current event
%%   will be removed from the stream.
%% in record form: #alt{tag, tp, mn, mx, rl}
%%

%% Attribute is a tuple {Name, SequenceNr, Optional, Type}
%% In record form: #att{nm, nr, opt, tp}
%%

%% To finalise the description of the Model we need to describe the
%% NamespaceMapping. This is a list of tuples {URI, Prefix}. The URI's are
%% the actual URIs used in the XML document. The Prefixes, however, don't have
%% to match with the prefixes used in the XML document, they can be selected
%% arbitrarily. The prefixes are used in the Tags in the model. In the model you
%% will find (as the Tag) "xns:h1" and in the NamespaceMapping
%% {'xh', "http://www.w3.org/1999/xhtml"}, while in the XML document you may
%% find an element '<xhtml:h1 xmlns:xhtml="http://www.w3.org/1999/xhtml">'.
%% in Record form: #ns{uri, pf}

%% ResultSoFar is a list of tuples of the same form as Currentstate, described
%% below. They describe the state of the higher levels of the state machine.
%% Each level corresponds (more or less) with the nesting-level of the XML
%% document: the outer level is processed by the highest level of the state
%% machine, and each next level is processed by a next level of the state
%% machine. When a level has been parsed completely, the result (and the
%% control) is passed back to the higher level state machine.
%%
%% CurrentState is a tuple {RemainingElements, ReceivedSoFar, ElementRecord,
%% RealElement}. It describes the state of the 'current' level of the state
%% machine.
%% in Record form: #cs{re, sf, er, rl}
%%
%% RemainingElements is initially a copy of the Elements list from the model for
%% the type that we are currently processing.  As we receive events
%% corresponding with elements, these are taken from the RemainingElement list.
%% In the end we should have an empty list, signalling that we have parsed all
%% elements of this type. At that moment we 'pop' up to the next higher level of
%% the state machine.  As mentioned above, the state of this higher level is
%% stored in the list 'ResultsSoFar' (it is the head of this list).
%%
%% In some cases there may be more instances of a certain element (MaxOccurs >
%% 1), therefore we only 'drop' the element at the head of the
%% 'RemainingElements' list when we receive a 'new' tag: with each start-tag
%% event we check whether this is the same element we are currently parsing. If
%% that is the case, we check whether one more is allowed (against MaxOccurs).
%% If we pass the limit we raise an exception. If we receive a new tag, we check
%% on MinOccurs and move to the next element of the RemainingElements list.
%%
%% Parsing of a new element can be one of two cases:
%% - if this is a simple element (Type={#PCDATA, ...}, some text between a start
%%   and an end-tag, or event an empty tag), then we expect to receive one (or
%%   in some cases several) 'character' events. This value will be assigned to
%%   the result-record (ElementRecord) at the position indicated by
%%   SequenceNumber.
%% - if this is another type, the current state will be appended to ResultSoFar
%%   and, using the description of the Type from the model, a new state
%%   (effectively a new level of the state machine) will be created.
%%
%% so far the description of the algorithm and its data structures...


-module(erlsom_parse).
-export([xml2StructCallback/2]).
-export([new_state/1]).
-export([new_state/2]).
-include("erlsom.hrl").
-include("exception.hrl").
-include("erlsom_sax.hrl").
-include("erlsom_parse.hrl"). %% the record definitions
-import(erlsom_lib, [convertPCData/4]).


%%%% lots of stuff to help debugging. Most of it only produces output if the process variable 'erlsom_debug'
%%%% is set. This makes it possible to suppress this output in parts of the test program that use erlsom for parsing
%%%% of a big configuration file.

%% debugFormat(Format, Args) ->
   %% case get(erlsom_debug) of
     %% _ ->
       %% io:format(Format, Args);
     %% _Else ->
       %% true
   %% end.

%% debug(Text) ->
   %% debugFormat("erlsom_parse: ~P\n", [Text, 20]).

%% debugMessage(Text) ->
   %% io:format("\nstatemachine: ~p\n", [Text]).
   %% %% debugFormat("\nstatemachine: ~p\n", [Text]).

%% debugEvent(S) ->
   %% io:format("event      : ~p\n", [S]).
   %% %% debugFormat("event      : ~p\n", [S]).

%% debugState(#state{currentState = Cs,
                  %% resultSoFar = Stack}) ->
  %% printCs(Cs, Stack),
  %% printStack(Stack).

%% printStack(Stack) ->
  %% debugFormat("stack      : ~p\n", [printResultSoFar(Stack)]).

%% printCs(undefined, _Stack) ->
  %% debugFormat("state      : undefined\n", []);

%% printCs(#cs{} = Cs, _Stack) ->
  %% printState(Cs);

%% printCs({'#PCDATA', _, Txt}, Stack) ->
  %% debugFormat("pcdata     : ~p\n", [Txt]),
  %% printTopElement(Stack);

%% printCs({'#text', _, Txt}, Stack) ->
  %% debugFormat("text       : ~p\n", [Txt]),
  %% printTopElement(Stack);

%% printCs(State, _Stack) ->
  %% debugFormat("state:     : ~P\n", [State, 5]).

%% printTopElement([]) ->
  %% debugFormat("next       : []\n", []);
%% printTopElement([Head | Tail]) ->
  %% printCs(Head, Tail).

%% printState(#cs{re = RemainingElements,
               %% er = ElementRecord,
               %% rl = Real,
               %% sf = SoFar,
               %% mxd = Mixed}) ->
  %% debugFormat("element rec: ~P\n", [ElementRecord, 10]),
  %% debugFormat("next elemts: ~p\n", [printRemainingElements(RemainingElements)]),
  %% debugFormat("mixed      : ~p, real: ~p, #so far: ~p\n", [Mixed, Real, SoFar]).

%% printRemainingElements(Re) ->
  %% printRemainingElements(Re, []).

%% printRemainingElements([], Acc) ->
  %% lists:reverse(Acc);
%% printRemainingElements([Head | Tail], Acc) ->
  %% case length(Acc) of
    %% X when X < 2 ->
      %% printRemainingElements(Tail, [fmtElement(Head) | Acc]);
    %% _ ->
      %% lists:reverse([etc | Acc])
  %% end.

%% fmtElement(#el{alts = Alternatives,
               %% mn = MinOccurs,
               %% mx = MaxOccurs}) ->
  %% {fmtAlternatives(Alternatives), {min, MinOccurs}, {max, MaxOccurs}}.

%% fmtAlternatives(Alts) ->
  %% fmtAlternatives(Alts, []).

%% fmtAlternatives([], Acc) ->
  %% lists:reverse(Acc);
%% fmtAlternatives([#alt{tag = Tag, tp = Type} | Tail], Acc) ->
  %% case length(Acc) of
    %% X when X < 2 ->
      %% fmtAlternatives(Tail, [{alt, {tag, Tag}, {type, Type}} | Acc]);
    %% _ ->
      %% lists:reverse([etc | Acc])
  %% end.

%%%% end of the debugging stuff

%% Creates a record as a tuple where the first element indicates the
%% type.
%% The second argument indicates whether we want to store "any-attributes".
newRecord(#type{nm = Type, nr = NrOfElements}, true) ->
  NewRecord = erlang:make_tuple(NrOfElements + 1, undefined),
  Record2 = setelement(1, NewRecord, Type),
  setelement(2, Record2, []);
newRecord(#type{nm = Type, nr = NrOfElements}, false) ->
  NewRecord = erlang:make_tuple(NrOfElements, undefined),
  setelement(1, NewRecord, Type).

%% to make it possible to use the callback without having
%% knowledge about the #state{} and #model{} records.
new_state(Model) ->
  new_state(Model, []).
new_state(#model{value_fun = ValFun} = Model, Namespaces) ->
  Nss = [#ns{prefix = Prefix, uri = Uri} || {Prefix, Uri} <- Namespaces],
  #state{model=Model, namespaces=Nss, value_fun = ValFun}.

%% This is the call-back function (called by the sax parser)
%% Filters out some events and calls the state machine.
xml2StructCallback(Event, State) ->

  %% debugState(State),
  %% debugEvent(Event),
  try
    case Event of
      startDocument ->
         stateMachine(Event, State);
      {startElement, _Uri, _LocalName, _Prefix, _Attributes} ->
         %% debug(Event),
         stateMachine(Event, State);
      {endElement, _Uri, _LocalName, _Prefix} ->
         stateMachine(Event, State);
      {characters, _Characters} ->
         stateMachine(Event, State);
      {ignorableWhitespace, Characters} ->
        case State of
          #state{currentState = #anyState{}} ->
            stateMachine({characters, Characters}, State);
          #state{currentState = {'#PCDATA', _, _}} ->
            stateMachine({characters, Characters}, State);
          #state{currentState = {'#text', _, _}} ->
            stateMachine({characters, Characters}, State);
          _ ->
            State
        end;
      {processingInstruction, _Target, _Data} ->  State;
      {startPrefixMapping, Prefix, URI} ->
         Namespaces = State#state.namespaces,
         State#state{namespaces = [#ns{prefix = Prefix, uri = URI} | Namespaces]};
      {endPrefixMapping, Prefix} ->
         case State of
           {result, _Result} ->
             State;
           _Else ->
             Namespaces = State#state.namespaces,
             State#state{namespaces = lists:keydelete(Prefix, 3, Namespaces)}
         end;
      endDocument ->
         case popNonRealElements(State) of
           {result, Result} ->
             Result;
           _Else ->
             throw({error, "unexpected end", (hd((_Else#state.currentState)#cs.re))#el.alts})
         end;
      {error, Message} ->
         throw(Message);
      {'EXIT', Message} ->
         exit(Message)
    end
  catch
    ?EXCEPTION(error, Reason, Stacktrace) -> throwError(error, {Reason, ?GET_STACK(Stacktrace)}, Event, State);
    Class:Exception -> throwError(Class, Exception, Event, State)
  end.

popNonRealElements(#state{currentState = #cs{rl = true}} = State) -> State;
popNonRealElements(State = #state{currentState = CS = #cs{re = [#el{mn = MinOccurs} | Tail], sf = ReceivedSoFar}})
    when ReceivedSoFar >= MinOccurs ->
    popNonRealElements(State#state{currentState = CS#cs{re = Tail, sf = 0}});
popNonRealElements(State = #state{currentState = #cs{re = [],
                                                     er = ElementRecord},
                                  value_fun = VFun,
                                  value_acc = Acc,
                                  resultSoFar = [Head | Tail]}) ->
    %% debugMessage("~nPop (after 'non-real' element)"),
    %% debugState(State),
    {NewCurrentState, Acc2} = pop(ElementRecord, Head, VFun, Acc),
    NewState = State#state{currentState = NewCurrentState,
        resultSoFar = Tail,
        value_acc = Acc2},
    popNonRealElements(NewState);
popNonRealElements(_State = #state{currentState = #cs{re = [],
                                                      er = ElementRecord},
                                   value_fun = ValueFun,
                                   value_acc = Acc,
                                   resultSoFar = []}) ->
    %% debugState(_State),
    applyValueFun(ValueFun, ElementRecord, Acc);
popNonRealElements(State) -> State.

%% the initial call can either be with a #state record, or with just the model.
stateMachine(Event, Model = #model{value_fun = ValueFun}) ->
  case Event of
    startDocument ->
      %% to make live a bit easier, copy value_fun from the
      %% model to the state record - a bit tricky...
      #state{model=Model, namespaces=[], value_fun = ValueFun};
    _Other ->
      throw({error, "unexpected event"})
  end;

%% start in _document state.
%%
%% The Model is a list of the form [Type].
%% Each Type is of the form {TypeName, TypeType (= sequence), [Element], Attributes, NrOfElements}.
%% Each Element is of the form {[Alternative], MinOccurs, MaxOccurs, SequenceNumber}.
%% Each Alternative is of the form {Tag, TypeReference, RealElement}.
%%
%% -record(state, {currentState, resultSoFar, model, namespaces}).
%% CurrentState is a tuple {RemainingElements, ReceivedSoFar, ElementRecord, RealElement}
stateMachine(Event,
             State = #state{currentState=undefined,
                            model = #model{tps = [#type{nm = '_document',
                                                        els = [#el{alts = Alternatives} | _]} | _],
                                           nss = NamespaceMapping,
                                           th = TypeHierarchy,
                                           any_attribs = StoreAnyAttr},
                            namespaces = Namespaces}) ->
  Types = (State#state.model)#model.tps,
  case Event of
    startDocument ->
      State;
    {startElement, Uri, LocalName, _Prefix, AttributeValues} ->
      Name = eventName(LocalName, Uri, NamespaceMapping),
      case lists:keysearch(Name, #alt.tag, Alternatives) of
        {value, #alt{tp = TypeReference, rl = RealElement}} ->
          %% look for the type description
          TheType = findType(RealElement, TypeReference, Types, AttributeValues, TypeHierarchy, Namespaces, NamespaceMapping),
          #type{tp = Tp, els = Elements2, mxd = Mixed} = TheType,
          %% create new record for this element &
          %% process the attributes
          if
            RealElement ->
              {NewRecord, _} = processAttributes(AttributeValues, TheType, newRecord(TheType, StoreAnyAttr),
                                            State#state.model, Namespaces, false);
            true ->
              NewRecord = newRecord(TheType, StoreAnyAttr)
          end,
          case Tp of
            sequence ->
              %% debug(NewRecord),
              NewCurrentState = #cs{re = Elements2, sf =0, er = NewRecord, rl = RealElement, mxd = Mixed};
            all ->
              NewCurrentState = #all{re = Elements2, er = NewRecord}
          end,
          %% create new state
          NewState = State#state{currentState = NewCurrentState,
                                 resultSoFar = []},
          if
            RealElement ->
              %% get the next event
              NewState;
            true ->  %% not sure whether this can really happen at this stage
              %% re-use the current event
              stateMachine(Event, NewState)
          end;
        _ ->
          throw({error, "unknown tag: " ++ LocalName})
      end;
    _YetAnother ->
      throw({error, "unexpected event"})
  end;


%% returns State
%% The Model is a list of the form [Type].
%% Each Type is of the form {TypeName, TypeType (= sequence), [Element], Attributes, NrOfElements}.
%% Each Element is of the form {[Alternative], MinOccurs, MaxOccurs, SequenceNumber}.
%% Each Alternative is of the form {Tag, TypeReference, RealElement}.

%% -record(state, {currentState, resultSoFar, model, namespaces}).
%% CurrentState is a tuple {RemainingElements, ReceivedSoFar, ElementRecord,
%% RealElement}.

stateMachine({characters, Characters},
             State = #state{currentState = #cs{re = [], mxd = true},
                            resultSoFar = ResultSoFar}) ->
  State#state{currentState = {'#text', 'char', Characters},
              resultSoFar = [State#state.currentState | ResultSoFar]};

%% RemainingElement = [] and RealElement = true -> should receive endElement now
%% TODO: could also be a text event in case of a mixed type?
stateMachine(Event, State = #state{currentState = #cs{re = [],
                                                      er = ElementRecord,
                                                      rl = true},
                                   resultSoFar = [Head | Tail],
                                   value_fun = VFun,
                                   value_acc = Acc}) ->
    %% debugMessage("pop"),
    %% debugState(State),
    %% debugEvent(Event),
  case Event of
    {endElement, _Uri, _LocalName, _Prefix} ->
      {NewCurrentState, Acc2} = pop(ElementRecord, Head, VFun, Acc),
      State#state{currentState = NewCurrentState,
                  resultSoFar = Tail,
                  value_acc = Acc2};
    _Else ->
      throw({error, {"1 - Unexpected event, expected end-tag"}})
  end;

stateMachine(Event, _State = #state{currentState = #cs{re = [],
                                                       er = ElementRecord,
                                                       rl = true},
                                    value_fun = ValueFun,
                                    value_acc = Acc,
                                    resultSoFar = []}) ->
    %% debugState(_State),
    %% debugEvent(Event),
  case Event of
    {endElement, _Uri, _LocalName, _Prefix} ->
      applyValueFun(ValueFun, ElementRecord, Acc);
    _Else ->
      throw({error, {"2 - Unexpected event, expected end-tag"}})
  end;


%% RemainingElement = [] and RealElement = false -> just 'pop', no end tag for
%% this type of 'pseudo-element'. Invoke the callback again with the new state
%% (and the same event).
%% TODO: this should also work for mixed types.
stateMachine(Event, State = #state{currentState = #cs{re = [],
                                                      er = ElementRecord,
                                                      rl = Real},
                                   value_fun = VFun,
                                   value_acc = Acc,
                                   resultSoFar = [Head | Tail]})
             when Real /= true ->
    %% debugMessage("~nPop (after 'non-real' element)"),
    %% debugState(State),
    %% debugEvent(Event),
    {NewCurrentState, Acc2} = pop(ElementRecord, Head, VFun, Acc),
    NewState = State#state{currentState = NewCurrentState,
                           resultSoFar = Tail,
                           value_acc = Acc2},
    stateMachine(Event, NewState);

stateMachine(Event, State = #state{currentState = #altState{name=Name, type=Type, real=Real,
                                                            receivedSoFar=Count, min=Min, max=Max, acc=Acc},
                                   resultSoFar  = [Head | Tail],
                                   model        = #model{tps = Types,
                                                         nss = NamespaceMapping,
                                                         th = TypeHierarchy,
                                                         any_attribs = StoreAnyAttr},
                                   value_fun = VFun,
                                   value_acc = ValueAcc,
                                   namespaces   = Namespaces}) ->
  %% debug(Event),
  case Event of %%{
    {startElement, Uri, LocalName, _Prefix, Attributes}  ->
    %% parse element
      ElementName = eventName(LocalName, Uri, NamespaceMapping),
      if %%{
        ElementName == Name ->
          %% check on MaxOccurs
          if %%{
            Max /= unbound, Count >= Max ->
              %% debug("But we have already received this event the maximum number of times"),
              %% pop.
              {NewCurrentState, Acc2} = pop(lists:reverse(Acc), Head, VFun, ValueAcc),
              NewState = State#state{currentState = NewCurrentState,
                                     resultSoFar = Tail,
                                     value_acc = Acc2},
              %% debug(NewState#state.currentState),
              %% debug(NewState),
              stateMachine(Event, NewState);
            true ->
              %% debug("Another event of this type is allowed"),
              case Type of %%{
                {'#PCDATA', PCDataType} ->
                  %% debug("receive text events"),
                  %% push the current status, create a new level in the state machine
                  State#state{currentState = {'#PCDATA', PCDataType, []},
                  resultSoFar = [State#state.currentState | State#state.resultSoFar]};
                _Else ->
                  %% not text: a complex type.
                  %% look for the type description
                  %% debug("Not text: a complex type"),
                  TypeDef = findType(Real, Type, Types, Attributes, TypeHierarchy, Namespaces, NamespaceMapping),
                  %% #type{els = Elements, atts = ListOfAttributes, nr = NrOfElements} = TypeDef,
                  %% create new record for this element
                  %% process the attributes
                  if
                    Real ->
                      {NewRecord, _} = processAttributes(Attributes, TypeDef, newRecord(TypeDef, StoreAnyAttr),
                                                         State#state.model, Namespaces, false);
                    true ->
                      NewRecord = newRecord(TypeDef, StoreAnyAttr)
                  end,
                  %% push the current status, create a new level in the state machine
                  NewState = State#state{currentState = #cs{re = TypeDef#type.els,
                                                            sf = 0,
                                                            er = NewRecord,
                                                            rl = Real},
                  resultSoFar = [State#state.currentState | State#state.resultSoFar]},
                  if %%{
                    Real ->
                      %% push the current status, create a new level in the state machine
                      NewState;
                    true ->
                      %% a helper-state. The tag we just received was NOT a start tag for a
                      %% corresponding enclosing element - there are no enclosing tags. The
                      %% tag just helped us to select the next type. Re-use the current event.
                      stateMachine(Event, NewState)
                  end %%}
              end %%}
          end; %%}
        true ->
          %% check on MinOccurs
          if %%{
            Count < Min ->
              throw({error, "Missing element before end-tag: " ++ LocalName});
            true ->
              {NewCurrentState, Acc2} = pop(lists:reverse(Acc), Head, VFun, ValueAcc),
              NewState = State#state{currentState = NewCurrentState,
                                     resultSoFar = Tail,
                                     value_acc = Acc2},
              stateMachine(Event, NewState)
          end %%}
       end; %%}

    {endElement, _Uri, LocalName, _Prefix} ->
      %% pop, don't remove the endElement from the input stream
      if %%{
        Count < Min ->
          throw({error, "Missing element before end-tag: " ++ LocalName});
        true ->
          {NewCurrentState, Acc2} = pop(lists:reverse(Acc), Head, VFun, ValueAcc),
          NewState = State#state{currentState = NewCurrentState,
                                 resultSoFar = Tail,
                                 value_acc = Acc2},
          stateMachine(Event, NewState)
      end; %%}

    _Else ->
      throw({error, {"Unexpected event", Event}})
  end; %%}


%% 'Any' type - should be replaced by something that returns the DOM tree
%% TODO: this should also work for mixed types(?)
stateMachine(Event, State = #state{currentState = (#anyState{anyInfo = (#anyInfo{ns = Namespace,
                                                                                 tns = Tns} = Ai)} = Cs),
                                   resultSoFar = [PreviousState | Tail]}) ->
  %% debugMessage("Any"),
  %% debugState(State),
  %% debugEvent(Event),
  case Event of
    {characters, _Characters} ->
       %% ignore (for the moment)
       State;

    {startElement, Uri, LocalName, _Prefix, _Attributes}  ->
       %% parse element  (recursively)
       %% check whether the namespace meets the requirements
       case Namespace of
         "##local" ->
           %% only on the first level do we have to check on the namespace etc.
           NewAnyInfo = Ai#anyInfo{ns = "##any"},
           NewCurrentState = Cs#anyState{anyInfo = NewAnyInfo},
           if
             Uri /= [] ->
                     throw({error, "Unexpected element in Any: " ++ LocalName});
             true ->
               ok
           end;
         "##other"->
           NewAnyInfo = Ai#anyInfo{ns = "##any"},
           NewCurrentState = Cs#anyState{anyInfo = NewAnyInfo},
           if
             Uri == [] ->
               throw({error, "Unexpected element (not namespace qualified): " ++ LocalName});
             %% Uri == Model#model.tns ->
             Uri == Tns ->
               throw({error, "Unexpected element (from target namespace): " ++ LocalName});
             true ->
               ok
           end;
         _ ->   %% ##any, or a list of namespaces - the list is not checked
           NewCurrentState = Cs
       end,
       %% push the current status, create a new level in the state machine
       State#state{currentState = NewCurrentState,
                   %% new currentState is still 'anyState', so no need to assign!
                   resultSoFar = [State#state.currentState | State#state.resultSoFar]};

    {endElement, _Uri, _LocalName, _Prefix} ->
       %% pop, remove the endElement from the input stream
       %% debug("pop"),
       %% debug(PreviousState),
       PreviousState2 = case PreviousState of
                          #cs{sf = Count} ->
                            %% debug("Increase Count"),
                            %% debug(Count+1),
                            PreviousState#cs{sf = Count +1};
                          #altState{receivedSoFar=Count} ->
                            PreviousState#altState{receivedSoFar=Count +1};
                          _ ->
                            PreviousState
                        end,
       State#state{currentState = PreviousState2,
                   resultSoFar = Tail};

    _Else ->
      throw({error, {"Unexpected event in Any", Event}})
  end;



%% Receive text events
stateMachine(Event, State = #state{currentState = {'#PCDATA', Type, TextSoFar},
                                   resultSoFar = [Head | Tail],
                                   model = #model{nss = NamespaceMapping},
                                   value_fun = ValueFun,
                                   value_acc = Acc,
                                   namespaces = Namespaces}) ->
  %% debugMessage("Receive text events"),
  %% debugState(State),
  %% debugEvent(Event),
  case Event of
    {characters, Characters} ->
      State#state{currentState = {'#PCDATA', Type, append(TextSoFar, Characters)}};
    {endElement, _Uri, _LocalName, _Prefix} ->
      ConvertedValue = try convertPCData(TextSoFar, Type, Namespaces, NamespaceMapping)
      catch
        _AnyClass:_Any ->
          throw({error, pp("Type mismatch: ~s/~p", [TextSoFar, Type])})
      end,
      NewCurrentState = insertValue(ConvertedValue, Head),
      %% debugFormat("new current state: ~p~n", [NewCurrentState]),
      if
        (Tail == []) and (NewCurrentState#cs.rl /= true) ->
          applyValueFun(ValueFun, NewCurrentState#cs.er, Acc);
        true ->
          State#state{currentState = insertValue(ConvertedValue, Head),
                      resultSoFar = Tail}
      end;
    _Else ->
      throw({error, {"3 - Unexpected event, expected end-tag", Event}})
  end;

%% Receive text events (in case of mixed elements)
stateMachine(Event, State = #state{currentState = {'#text', Type, TextSoFar},
                                   resultSoFar = [Head | Tail],
                                   model = #model{nss = NamespaceMapping},
                                   namespaces = Namespaces}) ->
  %% debugMessage("Receive text events (in case of mixed elements)"),
  %% debugState(State),
  %% debugEvent(Event),
  case Event of
    {characters, Characters} ->
      State#state{currentState = {'#text', Type, append(TextSoFar, Characters)}};
    _Else ->
      ConvertedValue = try convertPCData(TextSoFar, Type, Namespaces, NamespaceMapping)
      catch
        _AnyClass:_Any ->
          throw({error, "Wrong Type"})
      end,
      NewState = State#state{currentState = insertValue(ConvertedValue, Head),
                             resultSoFar = Tail},
      stateMachine(Event, NewState)
  end;

%% Event is the last event received from the sax parser
%% The Model is a list of the form [Type].
%% Each Type is of the form {TypeName, TypeType (= sequence), [Element], Attributes, NrOfElements}.
%% Each Element is of the form {[Alternative], MinOccurs, MaxOccurs, SequenceNumber}.
%% Each Alternative is of the form {Tag, TypeReference, RealElement, Min, Max}.
%%
%% State is a tuple {CurrentState, ResultSoFar, Model}
%% CurrentState is a tuple {RemainingElements, ReceivedSoFar, ElementRecord}

%% this one does the work
stateMachine(Event, State = #state{currentState = #cs{re = RemainingElements,
                                                      sf = ReceivedSoFar,
                                                      er = ElementRecord,
                                                      rl = RealElement,
                                                      mxd = Mixed},
                                   resultSoFar = ResultSoFar,
                                   model = #model{tps = Types,
                                                  nss = NamespaceMapping,
                                                  th = TypeHierarchy,
                                                  tns = Tns,
                                                  any_attribs = StoreAnyAttr},
                                   namespaces = Namespaces}) ->
  %% debugMessage("the work"),
  %% debugState(State),
  %% debugEvent(Event),
  [FirstElement | NextElements] = RemainingElements,
  #el{alts = Alternatives,
      mn = MinOccurs,
      mx = MaxOccurs,
      nillable = Nullable} = FirstElement,
  case Event of
    {startElement, Uri, LocalName, _Prefix, Attributes} ->
      Name = eventName(LocalName, Uri, NamespaceMapping),
      case lists:keysearch(Name, #alt.tag, Alternatives) of
        {value, #alt{tp = Type, rl = RealElement2, mn = Min, mx = Max}} ->
          %% This is the first or a second or third (or ...) instance of the element that we
          %% are 'working on'
          %% debug("This is a valid alternative"),
          if
            MaxOccurs /= unbound, ReceivedSoFar >= MaxOccurs ->
              %% debug("But we have already received this event the maximum number of times"),
              %% move on
              NewState = State#state{currentState = #cs{re = NextElements,
                                                        sf = 0,
                                                        er = ElementRecord,
                                                        rl = RealElement,
                                                        mxd = Mixed}},
              stateMachine(Event, NewState);
            true ->
              %% debug("Another event of this type is allowed"),
              if
                %% if max > 1, go to a new state that deals with these alternatives
                %% - #altState{name, type, real, receivedSoFar, acc, min, max, model, namespaces}
                Max > 1 ->
                  NewState = State#state{currentState = #altState{name=Name, type=Type, real=RealElement2,
                                                                  receivedSoFar = 0, min=Min, max=Max, acc=[]},
                                         resultSoFar = [State#state.currentState | ResultSoFar]},
                  stateMachine(Event, NewState);

                true ->
                  TypeDef = case Type of 
                            {'#PCDATA', _} ->
                              Type;
                            _ ->
                              findType(RealElement2, Type, Types, Attributes, TypeHierarchy, Namespaces, NamespaceMapping)
                            end,
                  case TypeDef of
                    {'#PCDATA', PCDataType} ->
                      %% debug("receive text events"),
                      %% push the current status, create a new level in the state machine
                      case (Nullable==true) andalso findNullableAttribute(Attributes) of
                        true ->
                          State#state{currentState = #cs{re = [], sf =0, er = nil, rl = true, mxd = false},
                                      resultSoFar = [State#state.currentState | ResultSoFar]};
                        _ ->
                          State#state{currentState = {'#PCDATA', PCDataType, []},
                                      resultSoFar = [State#state.currentState | ResultSoFar]}
                      end;
                    _Else ->
                      %% not text: a complex type.
                      %% look for the type description
                      %% debug("Not text: a complex type"),
                      %% Look for xsi:type attribute
                      #type{els = Elements, tp = Tp, mxd = Mxd} = TypeDef,
                      case RealElement2 of
                        true -> NewMxd = Mxd;
                        simple -> NewMxd = Mxd;
                        _ -> NewMxd = Mixed %% 'non-real' elements (groups) inherit this property
                      end,
                      %% create new record for this element
                      %% process the attributes
                      if
                        RealElement2 ->
                          {NewRecord, Nil} = processAttributes(Attributes, TypeDef, newRecord(TypeDef, StoreAnyAttr),
                                                               State#state.model, Namespaces, Nullable);
                        true ->
                          {NewRecord, Nil} = {newRecord(TypeDef, StoreAnyAttr), false}
                      end,
                      NewCurrentState =
                        if
                          Nil ->
                          %% Just need to get a closing tag (so no elements (re) below this one)
                          %% The value will be a record as usual - xsi:nil = true will be in
                          %% anyAttributes, and elements will be undefined. TODO: change this
                          %% into something that clearly indicates a nil value, but still allows for
                          %% attributes...
                          #cs{re = [], sf =0, er = {nil, NewRecord}, rl = RealElement2, mxd = NewMxd};
                        Tp == sequence ->
                          %% debug(NewRecord),
                          #cs{re = Elements, sf =0, er = NewRecord, rl = RealElement2, mxd = NewMxd};
                        Tp == all ->
                          #all{re = Elements, er = NewRecord}
                      end,
                      %% push the current status, create a new level in the state machine
                      NewState = State#state{currentState = NewCurrentState,
                                             resultSoFar = [State#state.currentState | ResultSoFar]},
                      if
                        RealElement2 ->
                          %% push the current status, create a new level in the state machine
                          NewState;
                        true ->
                          %% debug("a helper state"),
                          %% a helper-state. The tag we just received was NOT a start tag for a
                          %% corresponding enclosing element - there are no enclosing tags. The
                          %% tag just helped us to select the next type. Re-use the current event.
                          stateMachine(Event, NewState)
                      end
                  end
              end
            end;
        _Else ->
           %% See whether there is an 'Any' alternative.
           case Alternatives of
             [#alt{tag='#any', anyInfo = #anyInfo{ns = AltNs}}] when AltNs /= "##other" ->
               %% Add the elements of the model to the alternatives and try again
               AnyAlternatives = Alternatives ++ erlsom_lib:documentAlternatives(State#state.model),
               CurrentCurrentState = State#state.currentState,
               RemainingElementsWithAny = [FirstElement#el{alts = AnyAlternatives} | NextElements],
               CurrentStateWithAny = CurrentCurrentState#cs{re = RemainingElementsWithAny},
               NewState = stateMachine(Event, State#state{currentState = CurrentStateWithAny});
             _ ->
               %% note: if processContents (prCont) = "strict", then that 'doesn't count'
               %% because in that there should be a proper alternative. Arguably
               %% the 'any' alternative should not be part of the model in that case,
               %% but it is needed as an anchor when adding xsd's to the model.
               case lists:keysearch('#any', #alt.tag, Alternatives) of %{
                 {value, #alt{nxt = NextTags, tp = Type, anyInfo = AnyInfo, rl = RealElement2}}
                   when AnyInfo#anyInfo.prCont /= "strict" ->
                   %% If the tag is in NextTags, we have to move on
                   %% debug(NextTags),
                   case lists:member(Name, NextTags) of %{
                     true ->
                       NewState = undefined;
                     false ->
                       if  %{
                           MaxOccurs /= unbound, ReceivedSoFar >= MaxOccurs ->
                           %% can this be valid at all?
                           NewState = undefined;
                         true ->
                           %% push element
                           %% HERE
                           %% see whether this is a 'real' element.
                           if %{
                               RealElement2 ->
                                 %% need to check on 'anyInfo' here!!!!
                                 %% push the current status, create a new level in the state machine
                                 %% case matchesAnyInfo(Uri, AnyInfo, Tns) of
                                 case matchesAnyInfo(Uri, AnyInfo, Tns) of %{
                                   true ->
                                     NewState = State#state{currentState = #anyState{anyInfo = AnyInfo},
                                                            resultSoFar = [State#state.currentState | ResultSoFar]};
                                   false ->
                                     NewState = undefined
                                 end; %}
                               true ->  %% not a real element
                                 TypeDef = erlsom_lib:findType(Type, Types),
                                 %% debug(Type),
                                 #type{els = Elements, tp = Tp, mxd = Mxd} = TypeDef,
                                 NewRecord = newRecord(TypeDef, StoreAnyAttr),
                                 case Tp of
                                   sequence ->
                                     %% debug(NewRecord),
                                     NewCurrentState = #cs{re = Elements, sf =0, er = NewRecord, rl = RealElement2,
                                                           mxd = Mxd};
                                   all ->
                                     NewCurrentState = #all{re = Elements, er = NewRecord}
                                 end,
                                 %% push the current status, create a new level in the state machine
                                 NextState = State#state{currentState = NewCurrentState,
                                                         resultSoFar = [State#state.currentState | ResultSoFar]},
                                 %% a helper-state. The tag we just received was NOT a start tag for a
                                 %% corresponding enclosing element - there are no enclosing tags. The
                                 %% tag just helped us to select the next type. Re-use the current event.
                                 NewState = stateMachine(Event, NextState)
                           end %}
                       end %}
                   end; %}

                  _NotAny ->
                    %% debug(lists:keysearch('#any', #alt.tag, Alternatives)),
                    %% debug("no Any alternative"),
                    %% debug(Alternatives),
                    NewState = undefined
               end %}

           end, %}

           if
             NewState == undefined ->
             %% We received another element. First see if we should not have received more instances corresponding
             %% with the current 'state', and if that is ok, change to the next state (and try to match this event there)
             %% debug("Received another element"),
               if
                   ReceivedSoFar < MinOccurs ->
                       %% debug(Alternatives),
                       %% debug(Name),
                       %% debug(ReceivedSoFar),
                       %% debug(MinOccurs),
                       %% debugState(State),
                       throw({error, "Missing element before: " ++ LocalName});
                   true ->
                       %% debug("Moving on to the next state"),
                       NextState2 = State#state{currentState = #cs{re = NextElements,
                                                                   sf =0,
                                                                   er = ElementRecord,
                                                                   rl = RealElement,
                                                                   mxd = Mixed}},
                       stateMachine(Event, NextState2)
               end;
              true ->
                 NewState
           end
      end;

    {characters, Characters} ->
      %% debug("hier"),
      case lists:keysearch('#text', #alt.tag, Alternatives) of
        {value, #alt{tp = {'#PCDATA', PCDataType}, rl = Rl}} when Rl /= true ->
          %% Type == '#text' ->  receive text events
          %% This covers mixed elements - 'normal' text elements are processed when the start tag is
          %% received.
          State#state{currentState = {'#text', PCDataType, Characters},
                      resultSoFar = [State#state.currentState | ResultSoFar]};

        {value, #alt{tp = Type, rl = Rl}} when Rl /= true ->
          %% debug("odd case"),
          %% TODO: this seems to be a bit odd? What is this is a reference to a group?
          %% a helper element for this text
          %% look for the type description
          TypeDef = erlsom_lib:findType(Type, Types),
          %% create new record for this element
          %% (can't have any attributes)
          NewRecord = newRecord(TypeDef, StoreAnyAttr),
          %% push the current status, create a new level in the state machine
          %% (we know that this is a helperElement, therefore RealElement = false)
          NewState = State#state{currentState = #cs{re = TypeDef#type.els,
                                                    sf = 0,
                                                    er = NewRecord,
                                                    rl = Rl},
                                 resultSoFar = [State#state.currentState | ResultSoFar]},
          stateMachine(Event, NewState);

        _Else ->
          %% First see if we should not have received more instances corresponding
          %% with the current 'state', and if that is ok, change to the next state. (and try to match the
          %% event in that state).
          %% debug("text event not text alternative"),
          if
            Mixed == true ->
              %% debug("mixed == true"),
              State#state{currentState = {'#text', 'char', Characters},
                          resultSoFar = [State#state.currentState | ResultSoFar]};

              ReceivedSoFar < MinOccurs ->
              %% debug(ReceivedSoFar),
              %% debug(MinOccurs),
              %% debug(State#state.currentState),
              throw({error, "Missing element"});
          true ->
              %% debug("mixed == false"),
              %% debug(Mixed),
              NewState = State#state{currentState = #cs{re = NextElements,
                                                        sf = 0,
                                                        er = ElementRecord,
                                                        rl = RealElement,
                                                        mxd = Mixed}},
              stateMachine(Event, NewState)
          end
      end;

    {endElement, _Uri, LocalName, _Prefix} ->
      if
        ReceivedSoFar < MinOccurs ->
          throw({error, "Missing element before end-tag: " ++ LocalName});
        true ->
          %% (RemainingElements is not empty)
          %% Change to the next state (move one down the list), to make
          %% sure that we don't miss any non-optional elements
          NewState = State#state{currentState = #cs{re = NextElements,
                                                    sf = 0,
                                                    er = ElementRecord,
                                                    rl = RealElement,
                                                    mxd = Mixed}},
          stateMachine(Event, NewState)
      end;

    _Other ->
      throw({error, "Unexpected event"})
  end;

%% this one does the work for 'all'
stateMachine(Event, State = #state{currentState = #all{re = RemainingElements,
                                                       er = ElementRecord},
                                   resultSoFar = ResultSoFar,
                                   model = #model{tps = Types,
                                                  nss = NamespaceMapping,
                                                  th = TypeHierarchy,
                                                  any_attribs = StoreAnyAttr},
                                   value_fun = VFun,
                                   value_acc = Acc,
                                   namespaces = Namespaces}) ->
  %% debug(Event),
  case Event of
    {startElement, Uri, LocalName, _Prefix, Attributes}  ->
      Name = eventName(LocalName, Uri, NamespaceMapping),
      %% loop go through the list of elements, checking whether the
      %% tag occurs as an alternative in any one of them.
      %% and remove this element from the list of elements.
      case findTagInElements(Name, RemainingElements) of
        {#alt{tp = Type, rl = RealElement}, RemainingElements2, Nr, Nullable} ->
          NewCurrentState = #all{re = RemainingElements2, er = ElementRecord, nr = Nr},
          case Type of
            {'#PCDATA', PCDataType} ->
               %% receive text events
               %% push the current status, create a new level in the state machine
               case (Nullable==true) andalso findNullableAttribute(Attributes) of
                 true ->
                   State#state{currentState = #cs{re = [], sf =0, er = nil, rl = true, mxd = false},
                               resultSoFar = [NewCurrentState | ResultSoFar]};
                 _ ->
                   State#state{currentState = {'#PCDATA', PCDataType, []},
                               resultSoFar = [NewCurrentState | ResultSoFar]}
               end;
            _Else ->
               %% not text: a complex type.
               %% look for the type description
               %% Look for xsi:type attribute
               TypeDef = findType(RealElement, Type, Types, Attributes, TypeHierarchy, Namespaces, NamespaceMapping),
               #type{els = Elements, tp = Tp} = TypeDef,
               %% create new record for this element
               %% process the attributes
               if
                 RealElement ->
                   {NewRecord, _} = processAttributes(Attributes, TypeDef, newRecord(TypeDef, StoreAnyAttr),
                                                      State#state.model, Namespaces, false);
                 true ->
                   NewRecord = newRecord(TypeDef, StoreAnyAttr)
               end,
               case Tp of
                 sequence ->
                   NextState = #cs{re = Elements, sf =0, er = NewRecord, rl = RealElement};
                 all ->
                   NextState = #all{re = Elements, er = NewRecord}
               end,
               %% push the current status, create a new level in the state machine
               NewState = State#state{currentState = NextState,
                                      resultSoFar = [NewCurrentState| ResultSoFar]},
               if
                 RealElement ->
                   %% push the current status, create a new level in the state machine
                   NewState;
                 true ->
                   %% a helper-state. The tag we just received was NOT a start tag for a
                   %% corresponding enclosing element - there are no enclosing tags. The
                   %% tag just helped us to select the next type. Re-use the current event.
                   stateMachine(Event, NewState)
               end
          end;

        _Else ->
           %% We received another element.
           throw({error, "Unexpected event in All: " ++ LocalName})
      end;

    {characters, Characters} ->
       %% should be be extended to deal with mixed alements, or with simple elements with attributes?
       %% not sure...
         throw({error,
                pp("Unexpected characters event in All: ~s", [Characters])});

    {endElement, _Uri, LocalName, _Prefix} ->
      case lists:keysearch(1, #el.mn, RemainingElements) of
        {value, _} ->
          throw({error, "Missing element in All before end-tag: " ++ LocalName});
        _ ->
          case ResultSoFar of
            [] ->
              applyValueFun(VFun, ElementRecord, Acc);
            [PreviousState|Tail] ->
              {NewCurrentState, Acc2} = pop(ElementRecord, PreviousState, VFun, Acc),
              State#state{currentState = NewCurrentState, resultSoFar = Tail,
                          value_acc = Acc2}
          end
      end;

    _Other ->
      throw({error, "Unexpected event"})
  end.

%% skip means: no value fun has been provided.
pop(Value, State, skip, Acc) ->
  {pop2(Value, State), Acc};
pop(Value, State, VFun, Acc) ->
  {Value2, Acc2} = VFun(Value, Acc),
  {pop2(Value2, State), Acc2}.

%% 'pop': the result is in Value, move up one level in the state machine and put the result in the right
%% place in the ElementRecord at that level.
pop2(Value, State = #altState{receivedSoFar=Count, acc=Acc}) ->
   %% debug("pop to altState"),
  %% io:format("pop to altstate, value = ~p \n", [Value]),
   State#altState{receivedSoFar=Count +1, acc=[Value| Acc]};

pop2(Value, #cs{re = NewRemainingElements,
               sf = NewReceivedSoFar,
               er = NewElementRecord,
               mxd = Mixed} = NewState) ->
  #el{mx = MaxOccurs, nr = SequenceNr} = hd(NewRemainingElements),
  if
    (MaxOccurs > 1) or (Mixed == true) ->
      %% if maxOccurs > 1, the value is a list
        OldValue = element(SequenceNr, NewElementRecord),
        if
          OldValue == undefined ->  %% this can occur in the case of 'any' values that are not
                                    %% processed
            NewValue = [Value];
          true ->
            NewValue = OldValue ++ [Value]
       end,
       ElementRecord2 = setelement(SequenceNr, NewElementRecord, NewValue);
    true ->
      ElementRecord2 = setelement(SequenceNr, NewElementRecord, Value)
  end,
  %% debug("Pop"),
  NewState#cs{sf = NewReceivedSoFar + 1, er = ElementRecord2};

pop2(Value, State = #all{}) ->
  insertValue(Value, State).


%% put the text value into the list
insertValue(Value, #altState{receivedSoFar=Count, acc=Acc}) ->
   #altState{receivedSoFar=Count +1, acc=[Value| Acc]};


%% put the text value in the ElementRecord (at the right position)
insertValue(Value, #cs{re = RemainingElements,
                       sf = ReceivedSoFar,
                       er = ElementRecord,
                       rl = RealElement,
                       mxd = Mixed}) ->

  %% debug("insert value"),
  %% printState(Cs),
  [#el{mx = MaxOccurs, nr = SequenceNr}| _NextElements] = RemainingElements,
  if
    (MaxOccurs > 1) or (Mixed == true) ->
    %% if maxOccurs > 1, the value is a list
      case element(SequenceNr, ElementRecord) of
        undefined ->
          %% debugFormat("received so far: ~p\n", [ReceivedSoFar]),
          %% debugFormat("value: ~p\n", [element(SequenceNr + 2, ElementRecord)]),
          NewValue = [Value];
        CurrentValue ->
          %% debug(Mixed),
          %% debug(CurrentValue),
          NewValue = CurrentValue ++ [Value]
      end;
    true ->
      NewValue = Value
  end,

  ElementRecord2 = setelement(SequenceNr, ElementRecord, NewValue),
  #cs{re = RemainingElements,
      sf = if Mixed -> ReceivedSoFar; true -> ReceivedSoFar + 1 end,
      er = ElementRecord2, rl = RealElement, mxd = Mixed};

insertValue(Value, State = #all{er = ElementRecord, nr = SequenceNr}) ->
  ElementRecord2 = setelement(SequenceNr, ElementRecord, Value),
  State#all{er = ElementRecord2}.

%% translate the LocalName and the Uri to a kind of standardised name.
%%
%% TODO: There is a problem here if the schema is namespace qualified...
%% I guess there should be a lookup in the NamespaceMapping also if no
%% URI is present. As it is, elements without namespace may be accepted
%% when they should actually not be accepted. :(
eventName(LocalName, Uri, NamespaceMapping) ->
  try eventName2(LocalName, Uri, NamespaceMapping)
  catch
    %% list_to_existing_atom throws badarg if the atom doesn't exist
    error:badarg -> "not found"
  end.

eventName2(LocalName, _Uri = [], _NamespaceMapping) ->
  list_to_existing_atom(LocalName);

eventName2(LocalName, Uri, NamespaceMapping) ->
  case lists:keysearch(Uri, 2, NamespaceMapping) of
    {value, #ns{prefix = Prefix}}  ->
      case Prefix of
        undefined -> list_to_existing_atom(LocalName);
        _ -> list_to_existing_atom(Prefix ++ ":" ++ LocalName)
      end;
    _ ->
      specialEventName(LocalName, Uri)
  end.

specialEventName("type", "http://www.w3.org/2001/XMLSchema-instance") ->
  'xsi:type';

specialEventName(LocalName, Uri) ->
  case Uri of
   "http://www.w3.org/2001/XMLSchema-instance" ->
     list_to_existing_atom("xsi:" ++ LocalName);
   "http://www.w3.org/XML/1998/namespace" ->
     list_to_existing_atom("xml:" ++ LocalName);
   _ ->
     list_to_existing_atom(LocalName)
     %% throw({error, "unexpected namespace: " ++ Uri})
  end.

%% Goal:
%% - put the attribute values into the record
%% - check whether all required attributes are present
%%
%% Parameters:
%% Attributes: list of attribute records: localName, prefix, uri, value
%% ListOfAttribute: list of {name, optional (true/false), position in record}
%% Record: a record of right type for the element
%%
%% Returns:
%% {updated version of the Record, Null} where Null = true if nillable is true and an xsi:nil
%% attribute is found, false otherwise.
%%
%% Exceptions:
%% throws an error if attributes where missing, or if an unexpected attribute is found
%%
%% Procedure:
%% Process attributes one by one, remove processed attributes from the
%% ListOfAttributes, and, finally, see whether there are any non-optional
%% attributes left in the ListOfAttributes
processAttributes(Attributes, Type = #type{atts = ToReceive}, Record, Model, Namespaces, Nullable) ->
  processAttributes(Attributes, ToReceive, Type, Record, Model, Namespaces, Nullable, false).

processAttributes(_Attributes = [], ToReceive, _Type, Record, _Model, _Namespaces, _Nullable, Nil) ->
  checkAttributePresence(ToReceive),
  %% case catch checkAttributePresence(ToReceive) of
    %% true -> ok;
    %% _ ->
      %% debug(Type),
      %% throw({error, "remove me"})
  %% end,
  {Record, Nil};

processAttributes(_Attributes = [#attribute{localName=LocalName, uri=Uri, value=Value} | Tail], ListOfAttributes,
                  TypeDef, Record, Model = #model{nss = NamespaceMapping, any_attribs = StoreAnyAttr}, Namespaces, Nullable, Nil) ->
  Name = eventName(LocalName, Uri, NamespaceMapping),
  case lists:keysearch(Name, #att.nm, ListOfAttributes) of
    {value, #att{nr = Position, tp = Type}}  ->
      ConvertedValue = try convertPCData(Value, Type, Namespaces, NamespaceMapping)
      catch
        _AnyClass:_Any ->
          throw({error, "Wrong Type in value for attribute " ++ atom_to_list(Name)})
      end,
      processAttributes(Tail, lists:keydelete(Name, #att.nm, ListOfAttributes), TypeDef,
                        setelement(Position, Record, ConvertedValue),
                        Model, Namespaces, Nullable, Nil);
    _Else ->
      %% debug(Name),
      ConvertedValue = try convertPCData(Value, char, Namespaces, NamespaceMapping)
      catch
        _AnyClass:_Any ->
          throw({error, "Wrong Type in value for attribute " ++ LocalName})
      end,
      %% see whether the attribute is 'special'
      Nil2 = case {LocalName, Uri} of
        {"nil", "http://www.w3.org/2001/XMLSchema-instance"} ->
          %% see whether the element is 'nillable'
          case Nullable of
            true ->
              attributeIsTrue(ConvertedValue);
            _ ->
              throw({error, "Unexpected attribute: " ++ LocalName})
          end;

        {"schemaLocation", "http://www.w3.org/2001/XMLSchema-instance"} ->
          Nil;

        %% TODO: do something with this!!!
        {"space", "http://www.w3.org/XML/1998/namespace"} ->
          %% debug("got a space attrib"),
          Nil;

        {"noNamespaceSchemaLocation", "http://www.w3.org/2001/XMLSchema-instance"} ->
          Nil;

        {"type", "http://www.w3.org/2001/XMLSchema-instance"} ->
          Nil;

        _NotSpecial ->
          #type{anyAttr = AnyAttr} = TypeDef,
          %% see whether 'anyAttributes' was declared
          case AnyAttr of
            undefined ->
              %% debug(Value),
              %% debug(ListOfAttributes),
              %% debug(TypeDef),
              throw({error, "Unexpected attribute: " ++ LocalName});
            #anyAttr{ns = Namespace, tns = Tns} ->  %% 'processContents' is ignored ('skip' is assumed)
              %% no check whether the namespace meets the criteria
              case Namespace of
                "##local" ->
                  %% debug(Uri),
                  if
                    Uri /= [] ->
                                  throw({error, "Unexpected attribute: " ++ LocalName});
                    true ->
                      Nil
                  end;
                "##other"->
                  %% debug(Uri),
                  if
                    Uri == [] ->
                      throw({error, "Unexpected attribute (not namespace qualified): " ++ LocalName});
                    Uri ==  Tns ->
                      throw({error, "Unexpected attribute (from target NS): " ++ LocalName});
                    true ->
                      Nil
                  end;
                _ ->   %% ##any, or a list of namespaces - the list is not checked
                  Nil
              end
          end
      end,

      case StoreAnyAttr of
        true ->
          ListOfAttributes2 = [{{LocalName, Uri}, ConvertedValue} | element(2, Record)],
          processAttributes(Tail, ListOfAttributes, TypeDef,
                            setelement(2, Record, ListOfAttributes2), Model, Namespaces,
                            Nullable, Nil2);
        false ->
          processAttributes(Tail, ListOfAttributes, TypeDef, Record,
                            Model, Namespaces, Nullable, Nil2)
      end

  end.

%% Goal:
%% See whether the list contains non-optional attributes
%% If that is the case, the conclusion is that those were
%% missing from the XML document
%%
%% Parameters:
%% ListOfAttributes: list of {name, position in record, optional (true/false)}
%%
%% Returns:
%% true
%%
%% Exceptions:
%% throws an error if attributes where missing
checkAttributePresence(_ListOfAttributes = []) ->
  true;

checkAttributePresence(_ListOfAttributes = [#att{nm = Name, opt = false}  | _Tail]) ->
  throw({error, "Missing attribute: " ++ atom_to_list(Name)});

checkAttributePresence(_ListOfAttributes = [#att{opt = true}  | Tail]) ->
  checkAttributePresence(Tail).


throwError(Class, Exception, Event,
           #state{currentState = CurrentState,
                  resultSoFar = ResultSoFar}) ->
  %% Take the ElementRecord at current state, and print the first element
  Message = [{exception, Exception},
             %% for each of the elements in ResultSoFar,
             %% take the 'elementRecord' element and print the first element (the type).
             {stack, printStackTrace(CurrentState, ResultSoFar)},
             %% "Received: "
             {received, Event}],
  case Class of
    'error' -> exit({error, Message});
    'throw' -> throw({error, Message});
    'exit' -> exit({error, Message})
  end;

throwError(Class, Exception, _Event,
           _Something) ->
  case Class of
    'error' -> exit({error, Exception});
    'throw' -> throw({error, Exception});
    'exit' -> exit({error, Exception})
  end.

printStackTrace(CurrentState, ResultSoFar) ->
  [case CurrentState of
     #cs{er = ElementRecord} ->
       element(1, ElementRecord);
     {'#text', _Type, _TextSoFar} ->
        CurrentState;
     _ ->
       CurrentState
   end |
   printResultSoFar(ResultSoFar)].

printResultSoFar(undefined) ->
  [];

printResultSoFar(Elements) ->
  printResultSoFar(Elements, []).

printResultSoFar([], Acc) ->
  lists:reverse(Acc);
printResultSoFar([#cs{er = Er} | T], Acc) ->
  printResultSoFar(T, [element(1, Er) | Acc]);
printResultSoFar([#all{er = Er} | T], Acc) ->
  printResultSoFar(T, [element(1, Er) | Acc]);
printResultSoFar([_H | T], Acc) ->
  printResultSoFar(T, Acc).


%% loop go through the list of elements, checking whether the
%% tag occurs as an alternative in any one of them.
%% return the alternative and remove this element from the list of elements.
%% if the element is not found, return false
findTagInElements(Tag, Elements) ->
  findTagInElements(Tag, Elements, []).
findTagInElements(_Tag, [], _) -> false;
findTagInElements(Tag,
                  [Element = #el{alts = Alternatives, nr = Nr, nillable = Nillable} | NextElements],
                  PreviousElements) ->
      case lists:keysearch(Tag, #alt.tag, Alternatives) of
        {value, Alt = #alt{}} ->
          {Alt, PreviousElements ++ NextElements, Nr, Nillable};
        _Else ->
          findTagInElements(Tag, NextElements, [Element | PreviousElements])
      end.

%% Todo: last parameter is no longer used.
matchesAnyInfo(Uri, #anyInfo{ns = Namespace, tns = Tns}, _) ->
  case Namespace of
    "##local" ->
      if
        Uri /= [] ->
          false;
        true ->
          true
      end;
    "##other"->
      if
        Uri == [] ->
          false;
        Uri == Tns ->
          false;
        true ->
          true
      end;
    _ ->   %% ##any, or a list of namespaces - the list is not checked
      %% debug(Namespace),
      %% debug("TODO!!"),
      true
  end.

findNullableAttribute([]) ->
  false;
findNullableAttribute([#attribute{localName="nil", uri="http://www.w3.org/2001/XMLSchema-instance", value=Value} | _]) ->
  attributeIsTrue(Value);
findNullableAttribute([_Att | Tail]) ->
  findNullableAttribute(Tail).

attributeIsTrue("true") -> true;
attributeIsTrue("1") -> true;
attributeIsTrue(_) -> false.

append([], B2) when is_binary(B2) ->
    B2;
append(B1, B2) when is_binary(B1), is_binary(B2) ->
    <<B1/binary, B2/binary>>;
append(L1, L2) when is_list(L1), is_list(L2) ->
    lists:append(L1, L2).

pp(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

applyValueFun(skip, ElementRecord, _Acc) ->
  {result, ElementRecord};
applyValueFun(ValueFun, ElementRecord, Acc) ->
  {_, Result} = ValueFun(ElementRecord, Acc),
  {result, Result}.


findType(false, TypeReference, Types, _AttributeValues, _TypeHierarchy, _Namespaces, _NamespaceMapping) ->
    erlsom_lib:findType(TypeReference, Types);
findType(_, TypeReference, Types, AttributeValues, TypeHierarchy, Namespaces, NamespaceMapping) ->
    erlsom_lib:findType(TypeReference, Types, AttributeValues, TypeHierarchy, Namespaces, NamespaceMapping).
