%%% Copyright (C) 2006 - 2011 Willem de Jong
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
%%% A couple of functions used by erlsom_sax (for each encoding variant)
%%% ====================================================================

%%% Version: 20-01-2008

-module(erlsom_sax_lib).

-include("erlsom_sax.hrl").
-export([test/0]).
-export([findCycle/4]).
-export([continueFunK/4]).
-export([continueFunK/5]).
-export([continueFunK/6]).

-export([continueFun2K/7]).
-export([continueFun2K/5]).
-export([mapStartPrefixMappingCallback/3]).
-export([mapEndPrefixMappingCallback/3]).
-export([createStartTagEvent/3]).

%% there are 4 variants of this function, with different numbers of arguments
%% The names of the first arguments aren't really meaningful, they can
%% be anything - they are only there to be passed to 'ParseFun'.
continueFun2K(T, V1, V2, V3, State = #erlsom_sax_state{is_pull = true}, ParseFun, K) ->
    {continue, fun(Data) -> ParseFun(<<T/binary, Data/binary>>, V1, V2, V3, State#erlsom_sax_state{user_state = []}, K) end,
     lists:reverse(State#erlsom_sax_state.user_state)};
continueFun2K(T, V1, V2, V3, State, ParseFun, K) ->
  {Tail, ContinuationState2} = 
    (State#erlsom_sax_state.continuation_fun)(T, State#erlsom_sax_state.continuation_state),
  case Tail of 
    T -> throw({error, "Malformed: Unexpected end of data"});
    _ -> 
      ParseFun(Tail, V1, V2, V3, 
        State#erlsom_sax_state{continuation_state = ContinuationState2}, K)
  end.

continueFunK(Prefix, Head, T, State=#erlsom_sax_state{is_pull=true}, ParseFun, K) ->
  {continue, fun(Data) -> ParseFun(Prefix, Head, <<T/binary, Data/binary>>, State#erlsom_sax_state{user_state=[]}, K) end, 
    lists:reverse(State#erlsom_sax_state.user_state)};
continueFunK(Prefix, Head, T, State, ParseFun, K) ->
  {Tail, ContinuationState2} = 
    (State#erlsom_sax_state.continuation_fun)(T, State#erlsom_sax_state.continuation_state),
  case Tail of 
    T -> throw({error, "Malformed: Unexpected end of data"});
    _ -> 
      ParseFun(Prefix, Head, Tail, 
        State#erlsom_sax_state{continuation_state = ContinuationState2}, K)
  end.

continueFunK(Head, T, State=#erlsom_sax_state{is_pull=true}, ParseFun, K) ->
  {continue, fun(Data) -> ParseFun(Head, <<T/binary, Data/binary>>, State#erlsom_sax_state{user_state=[]}, K) end,
    lists:reverse(State#erlsom_sax_state.user_state)};
continueFunK(Head, T, State, ParseFun, K) ->
  {Tail, ContinuationState2} = 
    (State#erlsom_sax_state.continuation_fun)(T, State#erlsom_sax_state.continuation_state),
  case Tail of 
    T -> throw({error, "Malformed: Unexpected end of data"});
    _ -> 
      ParseFun(Head, Tail, 
        State#erlsom_sax_state{continuation_state = ContinuationState2}, K)
  end.

continueFun2K(T, Head, State = #erlsom_sax_state{is_pull=true}, ParseFun, K) ->
    {continue, fun(Data) -> ParseFun(<<T/binary, Data/binary>>, Head, State#erlsom_sax_state{user_state = []}, K) end,
     lists:reverse(State#erlsom_sax_state.user_state)};
continueFun2K(T, Head, State, ParseFun, K) ->
  {Tail, ContinuationState2} = 
    (State#erlsom_sax_state.continuation_fun)(T, State#erlsom_sax_state.continuation_state),
  case Tail of 
    T -> throw({error, "Malformed: Unexpected end of data"});
    _ -> 
      ParseFun(Tail, Head, 
        State#erlsom_sax_state{continuation_state = ContinuationState2}, K)
  end.

continueFunK(T, State = #erlsom_sax_state{is_pull=true}, ParseFun, K) ->
  {continue, fun(Data) -> ParseFun(<<T/binary, Data/binary>>, State#erlsom_sax_state{user_state=[]}, K) end,
    lists:reverse(State#erlsom_sax_state.user_state)};
continueFunK(T, State, ParseFun, K) ->
  {Tail, ContinuationState2} = 
    (State#erlsom_sax_state.continuation_fun)(T, State#erlsom_sax_state.continuation_state),
  case Tail of 
    T -> throw({error, "Malformed: Unexpected end of data"});
    _ -> 
      ParseFun(Tail, 
        State#erlsom_sax_state{continuation_state = ContinuationState2}, K)
  end.

 
%% function to call the Callback function for all elements in a list of 'new namespaces'.
%% returns State
mapStartPrefixMappingCallback([{Prefix, Uri} | Tail], State, Callback) ->
  mapStartPrefixMappingCallback(Tail, Callback({startPrefixMapping, Prefix, Uri}, State), Callback);
mapStartPrefixMappingCallback([], State, _Callback) ->
  State.

%% function to call the Callback function for all elements in a list of 'new namespaces'.
%% returns State
mapEndPrefixMappingCallback([{Prefix, _Uri} | Tail], State, Callback) ->
  mapEndPrefixMappingCallback(Tail, Callback({endPrefixMapping, Prefix}, State), Callback);
mapEndPrefixMappingCallback([], State, _Callback) ->
  State.


%% StartTag = {Prefix, LocalName, QualifiedName}
%% Attributes = list of Attribute
%% Attribute = {{Prefix, LocalName} Value}
%%
%% returns: {Name, Attributes2, NewNamespaces}
%% Name = {URI, LocalName, QualifiedName}
%% Attributes2 = list of Attribute2
%% Attribute2 = #attribute
%% NewNamespaces = list of {Prefix, URI} (prefix can be []).
%%
%% Namespaces are in such an order that namespace of the 'closest ancestors' 
%% are in front. That way the right element will be found, even if a prefix is 
%% used more than once in the document.
%%
createStartTagEvent(StartTag, Namespaces, Attributes) ->
  
  %% find the namespace definitions in the attributes
  {NewNamespaces, OtherAttributes} = lookForNamespaces([], [], Attributes),
  AllNamespaces = NewNamespaces ++ Namespaces,

  %% add the Uri to the tag name (if applicable)
  Name = tagNameTuple(StartTag, AllNamespaces),

  %% add the URIs to the attribute names (if applicable)
  Attributes2 = attributeNameTuples([], OtherAttributes, AllNamespaces),

  {Name, Attributes2, NewNamespaces}.

%% returns {Namespaces, OtherAttributes}, where 
%%   Namespaces = a list of tuples {Prefix, URI} 
%%   OtherAttributes = a list of tuples {Name, Value}
%%
lookForNamespaces(Namespaces, OtherAttributes, [Head | Tail]) ->
  {{Prefix, LocalName, _QName}, Value} = Head,
  if 
    Prefix == "xmlns" ->
      lookForNamespaces([{LocalName, Value} | Namespaces], 
                         OtherAttributes, Tail);
    Prefix == [],  LocalName == "xmlns" ->
      lookForNamespaces([{[], Value} | Namespaces], 
                        OtherAttributes, Tail);
    true -> 
      lookForNamespaces(Namespaces, [Head | OtherAttributes], Tail)
  end;
  
lookForNamespaces(Namespaces, OtherAttributes, []) -> 
  {Namespaces, OtherAttributes}.

%% StartTag = {Prefix, LocalName, QualifiedName} 
%% Namespaces = list of {Prefix, URI} (prefix can be []).
%%
%% Returns {Uri, LocalName, Prefix}
%%
%% TODO: error if not found? special treatment of 'xml:lang'?
tagNameTuple(StartTag, Namespaces) ->
  {Prefix, LocalName, _QName} = StartTag,
  case lists:keysearch(Prefix, 1, Namespaces) of
    {value, {Prefix, Uri}} -> {Uri, LocalName, Prefix};
    false -> {[], LocalName, Prefix}
  end.
      

%% Attributes = list of Attribute
%% Attribute = {{Prefix, LocalName} Value}
%% Namespaces = list of {Prefix, URI} (prefix can be []).
%%
%% Returns a list of #attribute records
attributeNameTuples(ProcessedAttributes, 
                    [{AttributeName, Value} | Attributes], Namespaces) ->
  {Uri, LocalName, Prefix} = attributeNameTuple(AttributeName, Namespaces),
  attributeNameTuples([#attribute{localName= LocalName,
                                  prefix = Prefix,
				  uri = Uri,
				  value = Value} | ProcessedAttributes], 
                      Attributes, Namespaces);

attributeNameTuples(ProcessedAttributes, [], _) ->
  ProcessedAttributes.

%% AttributeName = {Prefix, LocalName, QualifiedName}
%% Namespaces = list of {Prefix, URI} (prefix can be []).
%%
%% Returns {Uri, LocalName, Prefix}.
%% Difference with TagNameTuple: attributes without prefix do NOT belong
%% to the default namespace.
attributeNameTuple(AttributeName, Namespaces) ->
  {Prefix, LocalName, _} = AttributeName,
  if 
    Prefix == [] -> {[], LocalName, LocalName};
    true -> 
      case lists:keysearch(Prefix, 1, Namespaces) of
        {value, {Prefix, Uri}} ->
	    {Uri, LocalName, Prefix};
        false ->
            case Prefix of
              "xml" -> {"http://www.w3.org/XML/1998/namespace", LocalName, Prefix};
              _ -> {[], LocalName, Prefix}
            end
      end
  end.

%% simplistic function to find a cycle in a list [{a, b}, {b, c}, ...]
%% or if there is a path longer than MaxDepth.
%% The edge A, B is added; the rest of the graph is known
%% to be acyclical. So we start from B (To) and look for a path
%% to A (Current).
findCycle(To, Current, Edges, MaxDepth) ->
  findCycle(To, Current, Edges, MaxDepth, 1).

findCycle(_To, _Current, [], _MaxD, _CurrentD) ->
  false;
findCycle(To, Current, Edges, MaxD, CurrentD) ->
  %% take the next edge from edge from Current 
  case lists:keyfind(To, 1, Edges) of
    _ when MaxD == CurrentD ->
      max_depth; %% reached Max Depth
    false -> 
      false;
    {_, Current} ->
      cycle; %% found a cycle
    {_, B} ->
      RemainingEdges = lists:keydelete(To, 1, Edges),
      case findCycle(B, Current, RemainingEdges, MaxD, CurrentD + 1) of
        false ->
          findCycle(To, Current, RemainingEdges, MaxD, CurrentD);
        Other ->
          Other
      end
  end.

test() ->
  false = findCycle(b, a, [{a, b}], 2),
  max_depth  = findCycle(b, a, [{a, b}, {b, c}], 2),
  false = findCycle(b, a, [{a, b}, {b, c}], 3),
  false = findCycle(b, a, [{a, b}, {b, c}, {c, d}, {c, e}, 
                   {c, f}, {c, g}, {f, q}, {f, r}, {f, s},
                   {g, z}], 12),
  cycle  = findCycle(b, a, [{a, b}, {c, d}, {b, c}, {c, e}, 
                   {c, f}, {f, q}, {f, r}, {f, s}, {q, s},
                   {g, a}, {c, g}], 12),
  cycle  = findCycle(b, a, [{a, b}, {b, c}, {c, d}, {c, e}, 
                   {c, a}, {c, g}, {f, q}, {f, r}, {f, s},
                   {g, a}], 12).

