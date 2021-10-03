-module(erlsom_example_value).

%% output code that makes an example value for a type, using an erlsom model as input.
%%
%% example:
%%    #'p:Transaction'{
%%      'TransactionID' = "?",
%%      'SessionID' = "?",
%%      'CurrencyID' = "?",
%%      'Value' = "?"}
%%
%% In order to be able to embed the result it must be possible to specificy
%% indentation.

-include("erlsom_parse.hrl").

-export([from_model/2]).
-export([from_model/3]).
-export([test/1]).

-type options() :: [option()].
-type option() :: {indent, integer()} | {indent_level, integer()}.

-record(e_state, {
  indent :: integer(),
  level :: integer(),
  choice_depth = 0 :: integer()
}).

test(File) ->
  Options = [{include_any_attribs, false}],
  % generate hrl file, store in test_example.hrl
  erlsom:write_xsd_hrl_file(File, "test_example.hrl", Options),
  {ok, Model} = erlsom:compile_xsd_file(File, Options),
  % Pick a type from the model
  #model{tps = [#type{nm = '_document', els=Elements} | _]} = Model,
  [#el{alts = [#alt{tp = Type} | _]} | _] = Elements,
  % generate an example value,
  Example_value = from_model(Type, Model),
  file:write_file("test_example.erl",
                  [test_header(), Example_value, ".\n"]),
  %% See if it compiles
  compile:file("test_example.erl").

test_header() ->
  "-module(test_example).\n"
  "-export([go/0]).\n"
  "-include(\"test_example.hrl\").\n"
  "go() -> \n".

-spec from_model(Type::atom(), erlsom:model()) -> string().
from_model(Type, Model) ->
  from_model(Type, Model, []).

-spec from_model(Type::atom(), erlsom:model(), options()) -> string().
from_model(Type, Model, Options) ->
  State = #e_state{indent = proplists:get_value(indent, Options, 4),
                 level = proplists:get_value(indent_level, Options, 0)},
  from_type(Type, Model, State).

from_type(Type, #model{tps = Types} = Model, State) ->
  case lists:keyfind(Type, #type.nm, Types) of
    false ->
      throw({error, "Type not found", Type});
    Value ->
      from_type2(Value, Model, State)
  end.

from_type2(#type{nm = Name, els = Elements, atts = Attributes},
          #model{any_attribs = AnyAtts} = Model, State) ->
  Attribute_result = [from_attribute(A, Model, State) || A <- Attributes],
  Element_result = from_elements(Elements, Model, State),
  Fields =
    case AnyAtts of
      true ->
        Any_attribs = [[comment(State), indent(State), "    anyAttribs = []"]],
        ["{\n", add_commas(Any_attribs ++ Attribute_result ++ Element_result), $}];
      false ->
        ["{\n", add_commas(Attribute_result ++ Element_result), $}]
    end,
  [comment(State), indent(State), $#, atom_list(Name), Fields].

from_elements(Elements, Model, State) ->
  from_elements(Elements, Model, State, 0, []).

from_elements([], _Model, _State, _ChoiceCount, Acc) ->
  lists:reverse(Acc);
from_elements([E | T], Model, State, ChoiceCount, Acc) ->
  {Result, NewCount} =
    from_element(E, Model, State, ChoiceCount),
  from_elements(T, Model, State, NewCount, [Result | Acc]).


indent(#e_state{indent = Indent, level = Level}) ->
  lists:duplicate(Indent * Level, 32). % 32 = space.

bump_level(State) ->
  bump_level(State, 2).

bump_level(#e_state{level = Level} = State, Nr) ->
  State#e_state{level = Level + Nr}.

%% This is used between the alternatives - no comma's
%% because only one of them should be used, the others are commented
%% out.
add_breaks(List) ->
  separate(List, "\n").

add_commas(List) ->
  separate(List, ",\n").

separate([], _) ->
  [];
separate([H | T], Separator) ->
  separate(T, [H], Separator).

separate([], Acc, _)  ->
  lists:reverse(Acc);
separate([H | T], Acc, Separator) ->
  separate(T, [H, Separator | Acc], Separator).

from_attribute(#att{nm = Name, opt = Optional, tp = Type}, Model,
              State) ->
  Comment = case Optional of
              true ->
                [comment(State), indent(State), "    % Optional:\n"];
              false ->
                ""
            end,
  Value = default_value(Type, Model, State),
  [Comment, comment(State), indent(State), io_lib:format("    ~p = ~s", [Name, Value])].

from_element(#el{alts = Alternatives, mn = Min, mx = Max}, Model, State, Nr_choices) when
    length(Alternatives) == 1 ->
  Min_Max_comment = min_max_comment(Min, Max, State),
  Values = [from_alternative(A, Max, Model, State) || A <- Alternatives],
  {[Min_Max_comment, Values], Nr_choices};
from_element(#el{alts = Alternatives, mn = Min, mx = Max}, Model, State, Nr_choices) ->
  Unique_alternatives = lists:ukeysort(#alt.tp, Alternatives),
  Choice_comment = choice_comment(length(Unique_alternatives), State),
  Min_Max_comment = min_max_comment(Min, Max, State),
  %% If there are several tags that lead to 1 alternative, there may be more than 1
  %% #alt{} record for the same type.
  Alts = from_alternatives(Unique_alternatives, Model, State),
  Label = choice_label(Nr_choices),
  Result =
    case (Max > 1) of %% unbound > 1
      true ->
        [Min_Max_comment, comment(State), indent(State), "    ", Label, " = [\n",
         Choice_comment, add_breaks(Alts), $]];
      false ->
        [Min_Max_comment, comment(State), indent(State), "    ", Label, " = \n",
         Choice_comment, add_breaks(Alts)]
    end,
  {Result, Nr_choices + 1}.


choice_label(0) ->
  "choice";
choice_label(N) ->
  ["choice", integer_to_list(N)].

%% only used for alternatives of a choice
from_alternatives(Alternatives, Model, State) ->
  from_alternatives(Alternatives, Model, State, 1, []).

from_alternatives([], _, _, _, Acc) ->
  lists:reverse(Acc);
from_alternatives([H|T], Model, #e_state{choice_depth= Depth} = State,
                 Count, Acc) ->
  %% All alternatives are commented out, exacpt for the last one
  %% (the last one, because otherwise there are problems with commas, closing braces
  %% etc.).
  New_depth =
    case T of
      [] -> % no more alternatives, so this is the last one
        Depth;
      _ ->
        Depth + 1
    end,
  from_alternatives(T, Model, State, Count + 1,
                    [from_alternative2(H, Model,
                                       State#e_state{choice_depth = New_depth}) | Acc]).

from_alternative(#alt{tag = Tag, tp = Type, rl = Real, mn = _Min2, mx = _Max2},
                Max, Model, State) ->
  Field_name = name(Tag, Type, Real),
  %% add a newline if the type is a record
  Newline = newline(Type),
  %% add a relevant comment if the type is 'any':
  Any_comment = any_comment(Type, State),
  Value = default_value(Type, Model, State),
  Field = case (Max > 1) of %% unbound > 1
            true ->
              case Type of
                _ when Type == any; Type == '#ANY' ->
                  %% Note: this is not correct if MinOccurs > 0,
                  %% but that is rare, and it would be difficult
                  %% to figure out what to put in such a case.
                  io_lib:format("    ~p = ~s[]",
                                [Field_name, Newline]);
                _ ->
                  io_lib:format("    ~p = [~s~s]",
                                [Field_name, Newline, Value])
              end;
            false ->
              io_lib:format("    ~p = ~s~s", [Field_name, Newline, Value])
          end,
  [Any_comment, comment(State), indent(State), Field].

any_comment(any, State) ->
  [comment(State), indent(State), "    % Any value:\n"];
any_comment(_, _State) ->
  "".

%% inside a choice
from_alternative2(#alt{tp = Type, mn = Min, mx = Max}, Model, State) ->
  Value = default_value(Type, Model, State),
  Min_Max_comment = min_max_comment(Min, Max, bump_level(State, 1)),
  Field = case (Max > 1) of %% unbound > 1
            true ->
              Value2 = put_brace(Value),
              io_lib:format("~s]", [Value2]);
            false ->
              io_lib:format("~s", [Value])
          end,
  [Min_Max_comment, Field].

name(Tag, Type, Real) ->
  With_prefix =
    case Real of
           false ->
             case Type of
               {_,_} ->
                 Tag;
               _ ->
                Type
             end;
           _ ->
             Tag
    end,
  base_name(With_prefix).


%% the names of the fields should not have the prefix
base_name(Atom) when is_atom(Atom) ->
  String = atom_to_list(Atom),
  String_no_prefix = case string:tokens(String, ":") of
    [_Prefix, Name] ->
      Name;
    _ ->
      String
  end,
  list_to_atom(String_no_prefix).

comment(#e_state{choice_depth = D}) ->
  lists:duplicate(D, $%).


put_brace(String) ->
  Flat = lists:flatten(String),
  put_brace(Flat, []).

%% String = n * space + Something, must become:
%% n * space + [ + Something
put_brace([32 | T], Acc) ->
  put_brace(T, [32 | Acc]);
put_brace([$% | T], Acc) ->
  put_brace(T, [$% | Acc]);
put_brace([_ | _T] = Rest, Acc) ->
  [lists:reverse(Acc), $[, Rest].

min_max_comment(1, 1, _) ->
  "";
min_max_comment(0, 1, State) ->
  [comment(State), indent(State), "    % Optional:\n"];
min_max_comment(0, M, State) ->
  [comment(State), indent(State),
   io_lib:format("    % List with zero ~s elements:~n", [max_as_string(M)])];
min_max_comment(N, M, State) ->
  [comment(State), indent(State),
   io_lib:format("    % List with ~p ~s elements:~n", [N, max_as_string(M)])].

max_as_string(unbound) ->
  "or more";
max_as_string(N) ->
  "to " ++ integer_to_list(N).


choice_comment(1, _State) ->
  "";
choice_comment(N, State) ->
  [comment(State), indent(State),
   io_lib:format("        % Select one from the following ~p elements:~n", [N])].

newline({_, _}) ->
  % simple type
  [];
newline(any) ->
  [];
newline(_) ->
  "\n".

default_value({'#PCDATA', Type}, Model, State) ->
  default_value(Type, Model, State);
default_value(bool, _, _) ->
  "true";
default_value(any, _, _) ->
  "undefined";
default_value('#ANY', _, _) ->
  "\"could be anything\"";
default_value(qname, _, _) ->
  "qname";
default_value(integer, _, _) ->
  "42";
default_value({integer, long}, _, _) ->
  "42000";
default_value({integer, int}, _, _) ->
  "4200";
default_value({integer, short}, _, _) ->
  "420";
default_value({integer, byte}, _, _) ->
  "42";
default_value({integer, unsignedLong}, _, _) ->
  "43000";
default_value({integer, unsignedInt}, _, _) ->
  "4300";
default_value({integer, unsignedShort}, _, _) ->
  "430";
default_value({integer, unsignedByte}, _, _) ->
  "43";
default_value({integer, nonPositiveInteger}, _, _) ->
  "0";
default_value({integer, positiveInteger}, _, _) ->
  "42";
default_value({integer, negativeInteger}, _, _) ->
  "-42";
default_value({integer, nonNegativeInteger}, _, _) ->
  "0";
default_value(float, _, _) ->
  "3.1415927";
default_value(char, _, _) ->
  "\"?\"";
default_value(Type, Model, State) ->
  from_type(Type, Model, bump_level(State)).

atom_list(Atom) ->
  io_lib:format("~p", [Atom]).
