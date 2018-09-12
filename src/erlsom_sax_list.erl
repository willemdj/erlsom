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
%%% An XML parser, using the SAX model.
%%% ====================================================================

%% this file exists several times, but with different names: 
%% erlsom_sax_utf8, erlsom_sax_latin1 etc.
%% The only difference to the content of these 2 files is the definition below:
%% it can be UTF8, LAT1, LAT9, U16B or U16L. (The names have been chosen so that the 
%% number of bytes in the file will be the same in either case, so that it is 
%% easy to see whether the files are the same, although this check is obviously 
%% rather primitive.)

-define(LIST, true).
-ifdef(UTF8).
-module(erlsom_sax_utf8).
-define(BINARY, true).
-define(STR1(X), <<X>>).
-define(STR2(X1, X2), <<X1, X2>>).
-define(STR3(X1, X2, X3), <<X1, X2, X3>>).
-define(STR4(X1, X2, X3, X4), <<X1, X2, X3, X4>>).
-define(STR5(X1, X2, X3, X4, X5), <<X1, X2, X3, X4, X5>>).
-define(STR6(X1, X2, X3, X4, X5, X6), <<X1, X2, X3, X4, X5, X6>>).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), <<X1, X2, X3, X4, X5, X6, X7>>).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), <<X1, X2, X3, X4, X5, X6, X7, X8>>).
-define(DONTCARE_T(Y), <<_, Y/binary>>).
-define(STR1_T(X, Y), <<X, Y/binary>>).
-define(STR2_T(X1, X2, Y), <<X1, X2, Y/binary>>).
-define(STR3_T(X1, X2, X3, Y), <<X1, X2, X3, Y/binary>>).
-define(STR4_T(X1, X2, X3, X4, Y), <<X1, X2, X3, X4, Y/binary>>).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), <<X1, X2, X3, X4, X5, X6, X7, Y/binary>>).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, Y/binary>>).
-define(STR9_T(X1, X2, X3, X4, X5, X6, X7, X8, X9, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, X9, Y/binary>>).
-define(BOM1(X), <<16#EF, 16#BB, 16#BF, X/binary>>).
-define(BOM2, <<16#EF, 16#BB>>).
-define(BOM3, <<16#EF>>).
-endif.

-ifdef(U16B).
-module(erlsom_sax_utf16be).
-define(BINARY, true).
-define(STR1(X), <<0, X>>).
-define(STR2(X1, X2), <<0, X1, 0, X2>>).
-define(STR3(X1, X2, X3), <<0, X1, 0, X2, 0, X3>>).
-define(STR4(X1, X2, X3, X4), <<0, X1, 0, X2, 0, X3, 0, X4>>).
-define(STR5(X1, X2, X3, X4, X5), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5>>).
-define(STR6(X1, X2, X3, X4, X5, X6), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6>>).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7>>).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8>>).
-define(DONTCARE_T(Y), <<_, _, Y/binary>>).
-define(STR1_T(X, Y), <<0, X, Y/binary>>).
-define(STR2_T(X1, X2, Y), <<0, X1, 0, X2, Y/binary>>).
-define(STR3_T(X1, X2, X3, Y), <<0, X1, 0, X2, 0, X3, Y/binary>>).
-define(STR4_T(X1, X2, X3, X4, Y), <<0, X1, 0, X2, 0, X3, 0, X4, Y/binary>>).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, Y/binary>>).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, Y/binary>>).
-define(STR9_T(X1, X2, X3, X4, X5, X6, X7, X8, X9, Y), 
               <<0, X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, 0, X9, Y/binary>>).
-define(BOM1(X), <<16#FE, 16#FF, X/binary>>).
-define(BOM2, <<16#FE>>).
-define(BOM3, no_match).
-endif.

-ifdef(U16L).
-module(erlsom_sax_utf16le).
-define(BINARY, true).
-define(STR1(X), <<X, 0>>).
-define(STR2(X1, X2), <<X1, 0, X2, 0>>).
-define(STR3(X1, X2, X3), <<X1, 0, X2, 0, X3, 0>>).
-define(STR4(X1, X2, X3, X4), <<X1, 0, X2, 0, X3, 0, X4, 0>>).
-define(STR5(X1, X2, X3, X4, X5), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0>>).
-define(STR6(X1, X2, X3, X4, X5, X6), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0>>).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0>>).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, 0>>).
-define(DONTCARE_T(Y), <<_, _, Y/binary>>).
-define(STR1_T(X, Y), <<X, 0, Y/binary>>).
-define(STR2_T(X1, X2, Y), <<X1, 0, X2, 0, Y/binary>>).
-define(STR3_T(X1, X2, X3, Y), <<X1, 0, X2, 0, X3, 0, Y/binary>>).
-define(STR4_T(X1, X2, X3, X4, Y), <<X1, 0, X2, 0, X3, 0, X4, 0, Y/binary>>).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, Y/binary>>).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, 0, Y/binary>>).
-define(STR9_T(X1, X2, X3, X4, X5, X6, X7, X8, X9, Y), 
               <<X1, 0, X2, 0, X3, 0, X4, 0, X5, 0, X6, 0, X7, 0, X8, 0, X9, 0, Y/binary>>).
-define(BOM1(X), <<16#FF, 16#FE, X/binary>>).
-define(BOM2, <<16#FF>>).
-define(BOM3, no_match).
-endif.

-ifdef(LAT1).
-module(erlsom_sax_latin1).
-define(BINARY, true).
-define(STR1(X), <<X>>).
-define(STR2(X1, X2), <<X1, X2>>).
-define(STR3(X1, X2, X3), <<X1, X2, X3>>).
-define(STR4(X1, X2, X3, X4), <<X1, X2, X3, X4>>).
-define(STR5(X1, X2, X3, X4, X5), <<X1, X2, X3, X4, X5>>).
-define(STR6(X1, X2, X3, X4, X5, X6), <<X1, X2, X3, X4, X5, X6>>).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), <<X1, X2, X3, X4, X5, X6, X7>>).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), <<X1, X2, X3, X4, X5, X6, X7, X8>>).
-define(DONTCARE_T(Y), <<_, Y/binary>>).
-define(STR1_T(X, Y), <<X, Y/binary>>).
-define(STR2_T(X1, X2, Y), <<X1, X2, Y/binary>>).
-define(STR3_T(X1, X2, X3, Y), <<X1, X2, X3, Y/binary>>).
-define(STR4_T(X1, X2, X3, X4, Y), <<X1, X2, X3, X4, Y/binary>>).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), <<X1, X2, X3, X4, X5, X6, X7, Y/binary>>).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, Y/binary>>).
-define(STR9_T(X1, X2, X3, X4, X5, X6, X7, X8, X9, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, X9, Y/binary>>).
-define(BOM1(X), [no_match | X]).
-define(BOM2, no_match).
-define(BOM3, no_match2).
-endif.

-ifdef(LAT9).
-module(erlsom_sax_latin9).
-define(BINARY, true).
-define(STR1(X), <<X>>).
-define(STR2(X1, X2), <<X1, X2>>).
-define(STR3(X1, X2, X3), <<X1, X2, X3>>).
-define(STR4(X1, X2, X3, X4), <<X1, X2, X3, X4>>).
-define(STR5(X1, X2, X3, X4, X5), <<X1, X2, X3, X4, X5>>).
-define(STR6(X1, X2, X3, X4, X5, X6), <<X1, X2, X3, X4, X5, X6>>).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), <<X1, X2, X3, X4, X5, X6, X7>>).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), <<X1, X2, X3, X4, X5, X6, X7, X8>>).
-define(DONTCARE_T(Y), <<_, Y/binary>>).
-define(STR1_T(X, Y), <<X, Y/binary>>).
-define(STR2_T(X1, X2, Y), <<X1, X2, Y/binary>>).
-define(STR3_T(X1, X2, X3, Y), <<X1, X2, X3, Y/binary>>).
-define(STR4_T(X1, X2, X3, X4, Y), <<X1, X2, X3, X4, Y/binary>>).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), <<X1, X2, X3, X4, X5, X6, X7, Y/binary>>).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, Y/binary>>).
-define(STR9_T(X1, X2, X3, X4, X5, X6, X7, X8, X9, Y), <<X1, X2, X3, X4, X5, X6, X7, X8, X9, Y/binary>>).
-define(BOM1(X), [no_match | X]).
-define(BOM2, no_match).
-define(BOM3, no_match2).
-endif.

-ifdef(LIST).
-module(erlsom_sax_list).
-define(EMPTY, []).
-define(STR1(X), [X]).
-define(STR2(X1, X2), [X1, X2]).
-define(STR3(X1, X2, X3), [X1, X2, X3]).
-define(STR4(X1, X2, X3, X4), [X1, X2, X3, X4]).
-define(STR5(X1, X2, X3, X4, X5), [X1, X2, X3, X4, X5]).
-define(STR6(X1, X2, X3, X4, X5, X6), [X1, X2, X3, X4, X5, X6]).
-define(STR7(X1, X2, X3, X4, X5, X6, X7), [X1, X2, X3, X4, X5, X6, X7]).
-define(STR8(X1, X2, X3, X4, X5, X6, X7, X8), [X1, X2, X3, X4, X5, X6, X7, X8]).
-define(DONTCARE_T(Y), [_ | Y]).
-define(STR1_T(X, Y), [X | Y]).
-define(STR2_T(X1, X2, Y), [X1, X2 | Y]).
-define(STR3_T(X1, X2, X3, Y), [X1, X2, X3 | Y]).
-define(STR4_T(X1, X2, X3, X4, Y), [X1, X2, X3, X4 | Y]).
-define(STR7_T(X1, X2, X3, X4, X5, X6, X7, Y), [X1, X2, X3, X4, X5, X6, X7 |Y]).
-define(STR8_T(X1, X2, X3, X4, X5, X6, X7, X8, Y), [X1, X2, X3, X4, X5, X6, X7, X8 | Y]).
-define(BOM1(X), [65279 | X]).
-define(BOM2, no_match).
-define(BOM3, no_match2).
-endif.

-ifdef(BINARY).
-define(EMPTY, <<>>).
-endif.

%% these are only here to save some typing
-define(CF3K(A, B, K, C), erlsom_sax_lib:continueFunK(A, B, C, K)).
-define(CF4K(A, B, C, K, D), erlsom_sax_lib:continueFunK(A, B, C, D, K)).
-define(CF4_2K(A, B, C, K, D), erlsom_sax_lib:continueFun2K(A, B, C, D, K)).
-define(CF5K(A, B, C, D, K, E), erlsom_sax_lib:continueFunK(A, B, C, D, E, K)).
-define(CF6K(A, B, C, D, E, K, F), erlsom_sax_lib:continueFunK(A, B, C, D, E, F, K)).
-define(CF6_2K(A, B, C, D, E, K, F), erlsom_sax_lib:continueFun2K(A, B, C, D, E, F, K)).


-include("erlsom_sax.hrl").
-export([parse/2]).

parse(Xml, State) ->
  State2 = wrapCallback(startDocument, State),
  parseProlog(Xml, State2, fun(A) -> A end).

%% returns {State, Tail}
parseProlog(?EMPTY, State, K) ->
  ?CF3K(?EMPTY, State, K, fun parseProlog/3);
parseProlog(?STR1($<), State, K) ->
  ?CF3K(?STR1($<), State, K, fun parseProlog/3);
parseProlog(?STR2_T($<, $?, Tail), State, K) ->
    parseProcessingInstruction(Tail, State, fun(Tail2, State2) ->  parseProlog(Tail2, State2, K) end);
parseProlog(?STR2_T($<, $!, Tail) = T, State, K) ->
  case Tail of
    ?STR2_T($-, $-, Tail2) -> 
      parseComment(Tail2, State, fun(comment, Tail3, State2) ->
      parseProlog(Tail3, State2, K) end);
    ?STR7_T($D, $O, $C, $T, $Y, $P, $E, Tail2) -> 
      parseDTD(Tail2, State, fun(dtd, Tail3, State2) ->
      parseProlog(Tail3, State2, K) end);
    ?STR6($D, $O, $C, $T, $Y, $P) -> ?CF3K(T, State, K, fun parseProlog/3);
    ?STR5($D, $O, $C, $T, $Y) -> ?CF3K(T, State, K, fun parseProlog/3);
    ?STR4($D, $O, $C, $T) -> ?CF3K(T, State, K,  fun parseProlog/3);
    ?STR3($D, $O, $C) -> ?CF3K(T, State, K, fun parseProlog/3);
    ?STR2($D, $O) -> ?CF3K(T, State, K, fun parseProlog/3);
    ?STR1($D) -> ?CF3K(T, State, K, fun parseProlog/3);
    ?STR1($-) -> ?CF3K(T, State, K, fun parseProlog/3);
    ?EMPTY -> ?CF3K(T, State, K, fun parseProlog/3);
    _ -> throw({error, "Malformed: Illegal character in prolog"})
  end;
parseProlog(?STR1_T($<, Tail), State, K) ->
  parseContentLT(Tail, State, K);
%% whitespace in the prolog is ignored
parseProlog(?STR1_T(NextChar, Tail), State, K) 
  when ?is_whitespace(NextChar) ->
  parseProlog(Tail, State, K);
%% non-breaking space, used as byte order mark
parseProlog(?BOM1(Tail), State, K) ->
  parseProlog(Tail, State, K);
parseProlog(?BOM2, State, K) ->
  ?CF3K(?BOM2, State, K, fun parseProlog/3);
parseProlog(?BOM3, State, K) ->
  ?CF3K(?BOM3, State, K, fun parseProlog/3);
parseProlog(_Tail, _, _K) ->
  throw({error, "Malformed: Illegal character in prolog"}).

-ifdef(UTF8).
%% Decode the next character
%% Tail = the rest of the XML
%% returns {Char, Tail2, State2}
decodeChar(Tail, State, K) -> 
  case Tail of
    ?EMPTY -> ?CF3K(?EMPTY, State, K, fun decodeChar/3);
    <<C1, Tail2/binary>> when C1 < 16#80 ->
      K(C1, Tail2, State);
    <<C1, C2, Tail2/binary>> when C1 band 16#E0 =:= 16#C0,
                                  C2 band 16#C0 =:= 16#80 ->
      K(decode2(C1, C2), Tail2, State);
    <<C1>> when C1 band 16#E0 =:= 16#C0 ->
      ?CF3K(<<C1>>, State, K, fun decodeChar/3);
    <<C1, C2, C3, Tail2/binary>> when C1 band 16#F0 =:= 16#E0,
                                      C2 band 16#C0 =:= 16#80,
                                      C3 band 16#C0 =:= 16#80 ->
      K(decode3(C1, C2, C3), Tail2, State);
    <<C1, C2>> when C1 band 16#F0 =:= 16#E0,
                    C2 band 16#C0 =:= 16#80 ->
      ?CF3K(<<C1, C2>>, State,K, fun decodeChar/3);
    <<C1>> when C1 band 16#F0 =:= 16#E0 ->
      ?CF3K(<<C1>>, State, K, fun decodeChar/3);
    <<C1,C2,C3,C4, Tail2/binary>> when C1 band 16#F8 =:= 16#F0,
                                       C2 band 16#C0 =:= 16#80,
                                       C3 band 16#C0 =:= 16#80,
                                       C4 band 16#C0 =:= 16#80 ->
      K(decode4(C1, C2, C3, C4), Tail2, State);
    <<C1,C2,C3>> when C1 band 16#F8 =:= 16#F0,
                      C2 band 16#C0 =:= 16#80,
                      C3 band 16#C0 =:= 16#80 ->
      ?CF3K(<<C1, C2, C3>>, State, K, fun decodeChar/3);
    <<C1,C2>> when C1 band 16#F8 =:= 16#F0,
                   C2 band 16#C0 =:= 16#80 ->
      ?CF3K(<<C1, C2>>, State, K, fun decodeChar/3);
    <<C1>> when C1 band 16#F8 =:= 16#F0 ->
      ?CF3K(<<C1>>, State, K, fun decodeChar/3);
    <<_C1, _C2>> ->
      throw({error, "Decoding error: illegal UTF-8 encoding"})
  end.

%% decodes an UTF-8 encoded character that consists of 2 bytes.
decode2(C1, C2) ->
  case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
    C when 16#80 =< C ->
      C;
    _ ->
      %% Bad range.
      throw({error, "Decoding error: illegal UTF-8 encoding"})
  end.

decode3(C1, C2, C3) ->
  case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
       (C3 band 16#3F) of
    C when 16#800 =< C ->
      C;
    _ ->
      %% Bad range.
      throw({error, "Decoding error: illegal UTF-8 encoding"})
  end.

decode4(C1, C2, C3, C4) ->
  case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
       (C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
    C when 16#10000 =< C ->
      C;
    _ ->
      %% Bad range.
      throw({error, "Decoding error: illegal UTF-8 encoding"})
  end.
-endif.

-ifdef(U16B).
decodeChar(Tail, State, K) -> 
  case Tail of
    ?EMPTY -> ?CF3K(Tail, State, K, fun decodeChar/3);
    <<_>> -> 
      %% incomplete
      ?CF3K(Tail, State, K, fun decodeChar/3);
    <<C1, C2, Tail2/binary>> when C1 < 16#D8; C1 > 16#DF ->
      K(C1 * 256 + C2, Tail2, State);
    <<_Hi1, _Hi2, _Lo1>> -> 
      %% incomplete
      ?CF3K(Tail, State, K, fun decodeChar/3);
    <<Hi1, Hi2, Lo1, Lo2, Tail2/binary>> 
      when Hi1 >= 16#D8, Hi1 < 16#DC, Lo1 >= 16#DC, Lo1 < 16#E0 ->
        %% Surrogate pair
        Hi = Hi1 * 256 + Hi2,
        Lo = Lo1 * 256 + Lo2,
        Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
        K(Ch, Tail2, State);
    <<Hi1, _Hi2>> when Hi1 >= 16#D8, Hi1 < 16#DC ->
      %% Surrogate pair, incomplete
      ?CF3K(Tail, State, K, fun decodeChar/3);
    _ -> 
      {error,not_utf16be}
  end.
-endif.

-ifdef(U16L).
decodeChar(Tail, State, K) ->
  case Tail of
    ?EMPTY -> ?CF3K(Tail, State, K, fun decodeChar/3);
    <<_>> -> 
      %% incomplete
      ?CF3K(Tail, State, K, fun decodeChar/3);
    <<C1, C2, Tail2/binary>> when C2 < 16#D8; C2 > 16#DF ->
      K(C2 * 256 + C1, Tail2, State);
    <<_Hi1, _Hi2, _Lo1>> -> 
      %% incomplete
      ?CF3K(Tail, State, K,  fun decodeChar/3);
    <<Hi1, Hi2, Lo1, Lo2, Tail2/binary>> 
      when Hi2 >= 16#D8, Hi2 < 16#DC, Lo2 >= 16#DC, Lo2 < 16#E0 ->
        %% Surrogate pair
        Hi = Hi2 * 256 + Hi1,
        Lo = Lo2 * 256 + Lo1,
        Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
        K(Ch, Tail2, State);
    <<_Hi1, Hi2>> when Hi2 >= 16#D8, Hi2 < 16#DC ->
      %% Surrogate pair, incomplete
      ?CF3K(Tail, State, K, fun decodeChar/3);
    _ -> 
      {error,not_utf16le}
  end.
-endif.


-ifdef(LAT1).
decodeChar(Tail, State, K) ->
  case Tail of
    ?EMPTY -> ?CF3K(Tail, State, K,  fun decodeChar/3);
    ?STR1_T(C, T) -> K(C, T, State)
  end.
-endif.

-ifdef(LAT9).
decodeChar(Tail, State, K) ->
  case Tail of
    ?EMPTY -> ?CF3K(Tail, State, K, fun decodeChar/3);
    ?STR1_T(C, T) -> K(latin9toUnicode(C), T, State)
  end.

latin9toUnicode(16#A4) -> % EURO SIGN
  16#20AC;
latin9toUnicode(16#A6) -> % LATIN CAPITAL LETTER S WITH CARON
  16#0160;
latin9toUnicode(16#A8) -> % LATIN SMALL LETTER S WITH CARON
  16#0161;
latin9toUnicode(16#B4) -> % LATIN CAPITAL LETTER Z WITH CARON
  16#017D;
latin9toUnicode(16#B8) -> % LATIN SMALL LETTER Z WITH CARON
  16#017E;
latin9toUnicode(16#BC) -> % LATIN CAPITAL LIGATURE OE
  16#0152;
latin9toUnicode(16#BD) -> % LATIN SMALL LIGATURE OE
  16#0153;
latin9toUnicode(16#BE) -> % LATIN CAPITAL LETTER Y WITH DIAERESIS
  16#0178;
latin9toUnicode(Char) ->
  Char.
-endif.

-ifdef(LIST).
decodeChar(Tail, State, K) ->
  case Tail of
    ?EMPTY -> ?CF3K(Tail, State, K, fun decodeChar/3);
    ?STR1_T(C, T) -> K(C, T, State)
  end.
-endif.

%% returns {cdata, CData, Tail}
parseCDATA(Head, Tail0, State, K) ->
  case Tail0 of
    ?STR3_T($], $], $>, Tail) -> 
      K(cdata, lists:reverse(Head), Tail, State);
    ?STR2($], $]) -> 
      ?CF4K(Head, ?STR2($], $]), State, K, fun parseCDATA/4);
    ?STR1($]) -> 
      ?CF4K(Head, ?STR1($]), State, K, fun parseCDATA/4);
    ?STR1_T(NextChar, Tail) when NextChar < 16#80 ->
      parseCDATA([NextChar | Head], Tail, State, K);
    ?EMPTY -> 
      ?CF4K(Head, ?EMPTY, State, K,  fun parseCDATA/4);
    _ ->
      decodeChar(Tail0, State, fun(Char, Tail2, State2) ->
      parseCDATA([Char | Head], Tail2, State2, K) end)
  end.

%% returns {dtd, Tail} 
parseDTD(?STR1_T($[, Tail), State, K) ->
  parseIntSubset(Tail, State, fun(intSubset, Tail2, State2) ->
  parseDTD(Tail2, State2, K) end);
parseDTD(?STR1_T($>, Tail), State, K) ->
  K(dtd, Tail, State);
parseDTD(?DONTCARE_T(Tail), State, K) ->
  parseDTD(Tail, State, K);
parseDTD(?EMPTY, State, K) -> ?CF3K(?EMPTY, State, K, fun parseDTD/3).

%% returns {intSubset, Tail} 
parseIntSubset(?STR1_T($], Tail), State, K) ->
  K(intSubset, Tail, State);
parseIntSubset(?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  parseIntSubset(Tail, State, K);
%% get rid of whitespace
parseIntSubset(?STR8_T($<, $!, $E, $N, $T, $I, $T, $Y, Tail), State, K) ->
  parseEntity(Tail, State, fun(R) ->
   case R of
    {Tail2, State2} -> parseIntSubset(Tail2, State2, K);
    Other -> K(Other)
  end end);
parseIntSubset(?STR7($<, $!, $E, $N, $T, $I, $T) = T, State, K) -> ?CF3K(T, State, K, fun parseIntSubset/3);
parseIntSubset(?STR6($<, $!, $E, $N, $T, $I) = T, State, K) -> ?CF3K(T, State, K,  fun parseIntSubset/3);
parseIntSubset(?STR5($<, $!, $E, $N, $T) = T, State, K) -> ?CF3K(T, State, K,  fun parseIntSubset/3);
parseIntSubset(?STR4($<, $!, $E, $N) = T, State, K) -> ?CF3K(T, State, K, fun parseIntSubset/3);
parseIntSubset(?STR3($<, $!, $E) = T, State, K) -> ?CF3K(T, State, K, fun parseIntSubset/3);
parseIntSubset(?STR2($<, $!) = T, State, K) -> ?CF3K(T, State, K, fun parseIntSubset/3);
parseIntSubset(?STR1($<) = T, State, K) -> ?CF3K(T, State, K,  fun parseIntSubset/3);
%% comments (starting with <--)
parseIntSubset(?STR4_T($<, $!, $-, $-, Tail), State, K) -> 
  parseComment(Tail, State, fun(comment, Tail2, State2) ->
  parseIntSubset(Tail2, State2, K) end);
parseIntSubset(?STR3($<, $!, $-) = T, State, K) -> ?CF3K(T, State, K, fun parseIntSubset/3);
%% parameter entities (starting with %)
parseIntSubset(?STR1_T($%, Tail), State, K) -> %%
  parseReference([], parameter, Tail, State, fun({Head, Tail2, State2}) ->
  parseIntSubset(Head ++ Tail2, State2, K) end);
%% all other things starting with <
parseIntSubset(?STR1_T($<, Tail), State, K) -> 
  parseMarkupDecl(Tail, State, K); 
parseIntSubset(?EMPTY, State, K) -> ?CF3K(?EMPTY, State, K, fun parseIntSubset/3).

parseMarkupDecl(?STR1_T($>, Tail), State, K) -> 
  parseIntSubset(Tail, State, K);
parseMarkupDecl(?STR1_T($", Tail), State, K) -> %"
  {value, _, Tail2, State2} = parseLiteralValue(Tail, $", [], definition, State), %"
  parseMarkupDecl(Tail2, State2, K);
parseMarkupDecl(?STR1_T($', Tail), State, K) -> %'
  {value, _, Tail2, State2} = parseLiteralValue(Tail, $', [], definition, State), %'
  parseMarkupDecl(Tail2, State2, K);
parseMarkupDecl(?STR1_T(_, Tail), State, K) -> 
  parseMarkupDecl(Tail, State, K);
parseMarkupDecl(?EMPTY, State, K) -> ?CF3K(?EMPTY, State, K, fun parseMarkupDecl/3).


%% returns:
%% {Tail2, State2}, where the parsed entity has been added to the State
parseEntity(?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  parseEntity(Tail, State, K);
parseEntity(?STR1_T(NextChar, _) = Tail, State, K) when ?is_namestart_char(NextChar) ->
  parseEntityName(Tail, State, K);
parseEntity(?EMPTY, State, K) ->
  ?CF3K(?EMPTY, State, K, fun parseEntity/3);
parseEntity(Tail, State, K) ->
  parseEntityName(Tail, State, K).
  %% {Char, _Tail2, State2} = decodeChar(Tail, State),
  %% case Char of
    %% _ when ?is_namestart_char2(Char) ->
      %% parseEntityName(Tail, State2);
    %% _ ->
      %% throw({error, "Malformed: Illegal character in entity name"})
  %% end.

parseEntityName(Tail, State = #erlsom_sax_state{max_entity_size = MaxSize,
                                                max_nr_of_entities = MaxNr,
                                                output = OutputEncoding,
                                                entities = EntitiesSoFar,
                                                par_entities = ParEntitiesSoFar}, K) ->
  CurrentEntity = State#erlsom_sax_state.current_entity,
  getType(Tail, State, fun(Type, Tail2, State2) ->
  parseNameNoNamespaces(Tail2, State2, fun(Name, Tail3, State3) ->
  parseLiteral(definition, Tail3, State3#erlsom_sax_state{current_entity = Name}, fun(value, Value, Tail4, State4) ->
  if 
    length(EntitiesSoFar) + length(ParEntitiesSoFar) >= MaxNr ->
      throw({error, "Too many entities defined"});
    true ->
      ok
  end,
  %% this is a bit of a hack - parseliteral may return an encoded value, 
  %% but that is not what we want here.
  ValueAsList = case OutputEncoding of
                  'utf8' -> erlsom_ucs:decode_utf8(Value);
                  _ -> Value
                end,
  if
    length(ValueAsList) > MaxSize ->
      throw({error, "Entity too long"});
    true ->
      ok
  end,
  parseEndHook(Tail4, State4, fun(Tail5, State5) ->
  case Type of
    general ->
      State6 = State5#erlsom_sax_state{
        %% if an entity is declared twice, the first definition should be used.
        %% Therefore, add new ones to the back of the list.
        entities = EntitiesSoFar ++ [{Name, ValueAsList}],
        current_entity = CurrentEntity};
    parameter ->
      State6 = State5#erlsom_sax_state{
        par_entities = ParEntitiesSoFar ++ [{Name, ValueAsList}]}
  end,
  K({Tail5, State6}) end) end) end) end).

getType(?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  getType(Tail, State, K);
getType(?STR2_T($%, NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  K(parameter, Tail, State);
getType(?EMPTY, State, K) ->
  ?CF3K(?EMPTY, State, K, fun getType/3);
getType(Tail, State, K) ->
  K(general, Tail, State).


%% returns {comment, Tail}
parseComment(?STR1_T($-, Tail), State, K) ->
  case Tail of
    ?STR2_T($-, $>, Tail2) -> K(comment, Tail2, State);
    ?STR1($-) -> ?CF3K(?STR2($-, $-), State, K, fun parseComment/3);
    ?EMPTY -> ?CF3K(?STR1($-), State, K, fun parseComment/3);
    ?STR1_T($-, _) -> throw({error, "Malformed: -- not allowed in comment"});
    _ -> parseComment(Tail, State, K)
  end;
parseComment(?DONTCARE_T(Tail), State, K) ->
  parseComment(Tail, State, K);
parseComment(?EMPTY, State, K) ->
  ?CF3K(?EMPTY, State, K, fun parseComment/3).

%% returns {processinginstruction, Target, Data, Tail}
parseProcessingInstruction(Tail, State, K) ->
  parseNameNoNamespaces(Tail, State, fun(Target, Tail2, State2) ->
  parsePIData([], Tail2, State2,  fun(Data, Tail3, State3) ->
  State4 = wrapCallback({processingInstruction, Target, lists:reverse(Data)}, State3),
  K(Tail3, State4) end) end).

%% returns {Data, Tail}
parsePIData(Head, Tail, State, K) ->
  case Tail of
    ?STR2_T($?, $>, Tail2) -> K(Head, Tail2, State);
    ?STR1($?) -> ?CF4K(Head, ?STR1($?), State, K, fun parsePIData/4);
    ?STR1_T(NextChar, Tail2) when NextChar < 16#80 ->
      parsePIData([NextChar | Head], Tail2, State, K);
    ?EMPTY ->
      ?CF4K(Head, ?EMPTY, State,K, fun parsePIData/4);
    _ ->
      decodeChar(Tail, State, fun(Char, Tail2, State2) ->
      parsePIData([Char | Head], Tail2, State2, K) end)
  end.

%% function to call the Callback function for all elements in a list of 'new namespaces'.
%% returns State
mapStartPrefixMappingCallback([{Prefix, Uri} | Tail], State) ->
  mapStartPrefixMappingCallback(Tail, wrapCallback({startPrefixMapping, Prefix, Uri}, State));

mapStartPrefixMappingCallback([], State) ->
  State.

%% function to call the Callback function for all elements in a list of 'new namespaces'.
%% returns State
mapEndPrefixMappingCallback([{Prefix, _Uri} | Tail], State) ->
  mapEndPrefixMappingCallback(Tail, wrapCallback({endPrefixMapping, Prefix}, State));

mapEndPrefixMappingCallback([], State) ->
  State.

%% the '<' is already removed
%% returns {starttag, StartTag, Attributes, Tail}
%% or {emptyelement, StartTag, Attributes, Tail}
%%
%% where StartTag = {Prefix, LocalName, QualifiedName}
%%
parseStartTag(Tail, State, K) ->
  parseTagName(Tail, State, K).

%% parseTagName
%% returns {Name, Tail}, where
%% Name = {Prefix, LocalName, QualifiedName}
%%
%% To do: introduce a parameter that indicates whether we are using 
%% namespaces. 
parseTagName(?STR1_T(Char, Tail), State, K) 
  when ?is_namestart_char(Char) ->
  %% this should differentiate between 'with namespaces'and 'without'
  %% for the moment the assumption is 'with', therfore a name cannot
  %% start with a ':'.
  parseTagName([Char], Tail, State, K);
parseTagName(?EMPTY, State, K) ->
  ?CF3K(?EMPTY, State, K, fun parseTagName/3);
parseTagName(Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  case Char of
    _ when ?is_namestart_char2(Char) ->
      parseTagName([Char], Tail2, State2, K);
    _ ->
      throw({error, "Malformed: Illegal character in tag"})
  end end).

parseTagName(Head, ?STR1_T(NextChar, Tail), State, K) 
  when ?is_name_char(NextChar) ->
  parseTagName([NextChar | Head], Tail, State, K);
parseTagName(Head, ?STR1_T($:, Tail), State, K) ->
  parseTagName(Head, [], Tail, State, K);
parseTagName(Head, ?STR1_T($>, Tail), State, K) ->
  LocalName = lists:reverse(Head),
  K({starttag, {[], LocalName, LocalName}, [], Tail, State});
parseTagName(Head, ?STR2_T($/, $>, Tail), State, K) ->
  LocalName = lists:reverse(Head),
  K({emptyelement, {[], LocalName, LocalName}, [], Tail, State});
parseTagName(Head, ?STR1_T(NextChar, Tail), State, K) 
  when ?is_whitespace(NextChar) ->
  LocalName = lists:reverse(Head),
  parseAttributes({[], LocalName, LocalName}, [], Tail, State, K);
parseTagName(Head, ?STR1($/), State, K) ->
  ?CF4K(Head, ?STR1($/), State, K, fun parseTagName/4);
parseTagName(Head, ?EMPTY, State, K) ->
  ?CF4K(Head, ?EMPTY, State, K, fun parseTagName/4);
parseTagName(Head, Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  if 
    ?is_name_char2(Char) ->
      parseTagName([Char | Head], Tail2, State2, K);
    true ->
      throw({error, "Malformed: Illegal character in tag"})
  end end).

%% should there be another check on the first character of the local name?
parseTagName(Prefix, Head, ?STR1_T(NextChar, Tail), State, K)
  when ?is_name_char(NextChar) ->
  parseTagName(Prefix, [NextChar | Head], Tail, State, K);
parseTagName(Prefix, Head, ?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  parseAttributes({Pf, Hd, lists:append([Pf, ":", Hd])}, [], Tail, State, K);
parseTagName(Prefix, Head, ?STR1_T($>, Tail), State, K) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  K({starttag, {Pf, Hd, lists:append([Pf, ":", Hd])}, [], Tail, State});
parseTagName(Prefix, Head, ?STR2_T($/, $>, Tail), State, K) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  K({emptyelement, {Pf, Hd, lists:append([Pf, ":", Hd])}, [], Tail, State});
parseTagName(Prefix, Head, ?EMPTY, State, K) ->
  ?CF5K(Prefix, Head, ?EMPTY, State, K, fun parseTagName/5);
parseTagName(Prefix, Head, ?STR1($/), State, K) ->
  ?CF5K(Prefix, Head, ?STR1($/), State, K, fun parseTagName/5);
parseTagName(Prefix, Head, Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  if 
    ?is_name_char2(Char) ->
      parseTagName(Prefix, [Char | Head], Tail2, State2, K);
    true ->
      throw({error, "Malformed: Illegal character in tag"})
  end end).

parseAttrName(Head, ?STR1_T($:, Tail), State, K) ->
  %% Head is the prefix
  parseAttrName(Head, [], Tail, State, K);
parseAttrName(Head, ?STR1_T(NextChar, Tail), State, K)
  when ?is_name_char(NextChar) ->
  parseAttrName([NextChar | Head], Tail, State, K);
parseAttrName(Head, ?STR1_T($=, Tail), State, K) ->
  LocalName = lists:reverse(Head),
  K({[], LocalName, LocalName}, Tail, State);
parseAttrName(Head, ?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  LocalName = lists:reverse(Head),
  parseEqualSign(Tail, State, fun(Tail2, State2) ->
  K({[], LocalName, LocalName}, Tail2, State2) end);
parseAttrName(Head, ?EMPTY, State, K) ->
  ?CF4K(Head, ?EMPTY, State, K, fun parseAttrName/4);
parseAttrName(Head, Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  if 
    ?is_name_char2(Char) ->
      parseAttrName([Char | Head], Tail2, State2, K);
    true ->
      throw({error, "Malformed: Illegal character in attribute name"})
  end end).

%% should there be another check on the first character of the local name?
parseAttrName(Prefix, Head, ?STR1_T(NextChar, Tail), State, K)
  when ?is_name_char(NextChar) ->
  parseAttrName(Prefix, [NextChar | Head], Tail, State, K);
parseAttrName(Prefix, Head, ?STR1_T($=, Tail), State, K) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  K({Pf, Hd, lists:append([Pf, ":", Hd])}, Tail, State);
parseAttrName(Prefix, Head, ?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  Pf = lists:reverse(Prefix), 
  Hd = lists:reverse(Head), 
  parseEqualSign(Tail, State, fun(Tail2, State2) ->
  K({Pf, Hd, lists:append([Pf, ":", Hd])}, Tail2, State2) end);
parseAttrName(Prefix, Head, ?EMPTY, State, K) ->
  ?CF5K(Prefix, Head, ?EMPTY, State, K, fun parseAttrName/5);
parseAttrName(Prefix, Head, Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  if 
    ?is_name_char2(Char) ->
      parseAttrName(Prefix, [Char | Head], Tail2, State2, K);
    true ->
      throw({error, "Malformed: Illegal character in attribute name"})
  end end).

%% returns {Name, Tail, State}
parseNameNoNamespaces(?STR1_T(Char, Tail), State, K)
  when ?is_namestart_char(Char) ->
  parseNameNoNamespaces([Char], Tail, State, K);
parseNameNoNamespaces(Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  if 
    ?is_namestart_char2(Char) ->
      parseNameNoNamespaces([Char], Tail2, State2, K);
    true ->
      throw({error, "Malformed: Illegal character in name"})
  end end).
      
parseNameNoNamespaces(Head, ?STR1_T(NextChar, Tail), State, K)
  when ?is_name_char(NextChar) ->
  parseNameNoNamespaces([NextChar | Head], Tail, State, K);
parseNameNoNamespaces(Head, ?STR1_T($:, Tail), State, K) ->
  parseNameNoNamespaces([$: | Head], Tail, State, K);
parseNameNoNamespaces(Head, T = ?STR1_T($>, _), State, K) ->
  K(lists:reverse(Head), T, State);
parseNameNoNamespaces(Head, T = ?STR1_T($?, _), State, K) ->
  K(lists:reverse(Head), T, State);
parseNameNoNamespaces(Head, T = ?STR1_T($=, _), State, K) ->
  K(lists:reverse(Head), T, State);
parseNameNoNamespaces(Head, T = ?STR1_T(NextChar, _), State, K)
  when ?is_whitespace(NextChar) ->
  K(lists:reverse(Head), T, State);
parseNameNoNamespaces(Head, ?EMPTY, State, K) ->
  ?CF4K(Head, ?EMPTY, State, K, fun parseNameNoNamespaces/3);
parseNameNoNamespaces(Head, Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  if 
    ?is_name_char2(Char) ->
      parseNameNoNamespaces([Char | Head], Tail2, State2, K);
    true ->
      throw({error, "Malformed: Illegal character in name"})
  end end).

%% returns: {attributes, Attributes, Tail}}
%% Attributes = list of {Name, Value} tuples, and
%% Name = {Prefix, LocalName, QualifiedName}.
parseAttributes(StartTag, Attributes, ?STR1_T($>, Tail), State, K) ->
  K({starttag, StartTag, Attributes, Tail, State});
parseAttributes(StartTag, Attributes, ?STR1($/), State, K) ->
  ?CF5K(StartTag, Attributes, ?STR1($/), State,K, fun parseAttributes/5);
parseAttributes(StartTag, Attributes, ?STR2_T($/, $>, Tail), State, K) ->
  K({emptyelement, StartTag, Attributes, Tail, State});
parseAttributes(StartTag, Attributes, ?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  parseAttributes(StartTag, Attributes, Tail, State, K);
parseAttributes(StartTag, Attributes, ?STR1_T(NextChar, Tail), State, K) 
  when ?is_namestart_char(NextChar) ->
    parseAttrName([NextChar], Tail, State, fun(AttributeName, Tail2, State2) ->
    %% {attribute, Attribute, Tail3, State3} = 
      %% parseAttribute([NextChar], Tail, State),
    parseLiteral(attribute, Tail2, State2, fun(value, Value, Tail3, State3) ->
      %% {attribute, {AttributeName, Value}, Tail2, State2};
      %% parseAttributeValue(AttributeName, Tail2, State2),
    parseAttributes(StartTag, [{AttributeName, Value} | Attributes], Tail3, State3, K) end) end);
parseAttributes(StartTag, Attributes, ?EMPTY, State, K) ->
  ?CF5K(StartTag, Attributes, ?EMPTY, State, K, fun parseAttributes/5);
parseAttributes(StartTag, Attributes, Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  case Char of 
    _ when ?is_namestart_char2(Char) ->
      parseAttrName([Char], Tail2, State2, fun(AttributeName, Tail3, State3) ->
      parseLiteral(attribute, Tail3, State3, fun(value, Value, Tail4, State4) ->
      %% {attribute, Attribute, Tail3, State3} = 
      %% parseAttribute([Char], Tail2, State2),
      parseAttributes(StartTag, [{AttributeName, Value} | Attributes], Tail4, State4, K) end) end);
    _ ->
      throw({error, "Malformed: Illegal character in name"})
  end end).

  
%% returns {value, Value, Tail, State}
%% depending on the context (attribute or definition) the
%% handling of entities is slightly different.
parseLiteral(Context, ?STR1_T($", Tail), State, K) -> %"
  parseLiteralValue(Tail, $", Context, State, K); %"
parseLiteral(Context, ?STR1_T($', Tail), State, K) -> %'  
  parseLiteralValue(Tail, $', Context, State, K); %'
parseLiteral(Context, ?STR1_T(NextChar, Tail), State, K) 
  when ?is_whitespace(NextChar) ->
  parseLiteral(Context, Tail, State, K);
parseLiteral(Context, ?EMPTY, State, K) -> 
  ?CF4K(Context, ?EMPTY, State, K, fun parseLiteral/4);
parseLiteral(_C, _T, _, _K) ->
  throw({error, "Malformed: Illegal character in literal value"}).

%% TODO: this can be generalized, for example parsing up to the = sign 
%% in an attribute value is exactly the same.
parseEndHook(?STR1_T($>, Tail), State, K) ->
  K(Tail, State);
parseEndHook(?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  parseEndHook(Tail, State, K);
parseEndHook(?EMPTY, State, K) -> 
  ?CF3K(?EMPTY, State, K, fun parseEndHook/3);
parseEndHook(_Tail, _, _K) ->
  throw({error, "Malformed: Illegal character in entity definition"}).

parseEqualSign(?STR1_T($=, Tail), State, K) ->
  K(Tail, State);
parseEqualSign(?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) ->
  parseEqualSign(Tail, State, K);
parseEqualSign(?EMPTY, State, K) -> 
  ?CF3K(?EMPTY, State, K, fun parseEqualSign/3);
parseEqualSign(_Tail, _, _K) ->
  throw({error, "Malformed: Illegal character in attribute name"}).

%% previous char was '<'
parseContentLT(?STR1_T($!, Tail), State, K) ->
  case Tail of
    ?STR2_T($-, $-, Tail3) ->
      parseComment(Tail3, State, fun(comment, Tail4, State2) ->
      parseContent(Tail4, State2, K) end);
    ?STR1($-) ->
      ?CF3K(?STR2($!, $-), State, K,  fun parseContentLT/3);
    ?EMPTY ->
      ?CF3K(?STR1($!), State, K, fun parseContentLT/3);
    ?STR7_T($[, $C, $D, $A, $T, $A, $[, Tail3) ->
      parseCDATA([], Tail3, State, fun(cdata, CData, Tail4, State2) ->
      %% call callback -
      %% If Cdata is preceded and/or followed by text there will be 2 or 3 
      %% events, but that is legal according to the sax doc.
      State3 = wrapCallback({characters, encodeOutput(CData, State)}, State2),
      parseContent(Tail4, State3, K) end);
    ?STR6($[, $C, $D, $A, $T, $A) ->
      ?CF3K(?STR7($!, $[, $C, $D, $A, $T, $A), State, K,  fun parseContentLT/3);
    ?STR5($[, $C, $D, $A, $T) ->
      ?CF3K(?STR6($!, $[, $C, $D, $A, $T), State, K, fun parseContentLT/3);
    ?STR4($[, $C, $D, $A) ->
      ?CF3K(?STR5($!, $[, $C, $D, $A), State, K, fun parseContentLT/3);
    ?STR3($[, $C, $D) ->
      ?CF3K(?STR4($!, $[, $C, $D), State, K, fun parseContentLT/3);
    ?STR2($[, $C) ->
      ?CF3K(?STR3($!, $[, $C), State, K, fun parseContentLT/3);
    ?STR1($[) ->
      ?CF3K(?STR2($!, $[), State, K, fun parseContentLT/3)
  end;

parseContentLT(?STR1_T($?, Tail), State, K) ->
   parseProcessingInstruction(Tail, State, fun(Tail2, State2) -> parseContent(Tail2, State2, K) end);

parseContentLT(?STR1_T($/, Tail), State, K) ->
  %% this should be the endTag
  [{QName, Uri, LocalName, Prefix, OldNamespaces, NewNamespaces} | EndTags2] = 
    State#erlsom_sax_state.endtags,
  parseEndTag(Tail, QName, State, fun(X)->
   case X of
    {ok, Tail3, State2} -> 
      %% Call the call back functions for the end tag
      State3 = wrapCallback({endElement, Uri, LocalName, Prefix}, State2),
      State4 = mapEndPrefixMappingCallback(NewNamespaces, State3),
      State5 = State4#erlsom_sax_state{namespaces = OldNamespaces, endtags = EndTags2},
      parseContent(Tail3, State5, K);
    error -> 
      throw({error, "Malformed: Tags don't match"})
  end end);

parseContentLT(?EMPTY, State, K) ->
  ?CF3K(?EMPTY, State, K, fun parseContentLT/3);

parseContentLT(Tail, State, K) ->
  Namespaces = State#erlsom_sax_state.namespaces,
  parseStartTag(Tail, State, fun(X) ->
    case X of
    {emptyelement, {Prefix, _LocalName, _QName}=StartTag, Attributes, Tail2, State2} ->
      {{Uri, LocalName, QName}, Attributes2, NewNamespaces} = 
        createStartTagEvent(StartTag, Namespaces, Attributes),
      %% Call the call back functions
      State3 = mapStartPrefixMappingCallback(NewNamespaces, State2),
      State4 = wrapCallback({startElement, Uri, LocalName, Prefix, Attributes2}, State3),
      State5 = wrapCallback({endElement, Uri, LocalName, QName}, State4),
      State6 = mapEndPrefixMappingCallback(NewNamespaces, State5),
      parseContent(Tail2, State6, K);
    {starttag, {Prefix, _LocalName, QName} = StartTag, Attributes, Tail2, State2} ->
      EndTags = State#erlsom_sax_state.endtags,
        {{Uri, LocalName, Prefix}, Attributes2, NewNamespaces} = 
      createStartTagEvent(StartTag, Namespaces, Attributes),
      %% Call the call back function
      State3 = mapStartPrefixMappingCallback(NewNamespaces, State2),
      State4 = wrapCallback({startElement, Uri, LocalName, Prefix, Attributes2}, State3),
      State5 = State4#erlsom_sax_state{namespaces = NewNamespaces ++ Namespaces, 
                     endtags = [{QName, Uri, LocalName, Prefix, Namespaces, NewNamespaces} | EndTags]},
      %% TODO: check the order of the namespaces
      parseContent(Tail2, State5, K)
     end end).

parseContent(?STR1_T($<, Tail), #erlsom_sax_state{endtags = EndTags} = State, K) when EndTags /= [] ->
  parseContentLT(Tail, State, K);

parseContent(?EMPTY, #erlsom_sax_state{endtags = EndTags} = State, K) ->
  case EndTags of 
    [] ->
      parseEndDocument(?EMPTY, State, K);
    _ ->
      ?CF3K(?EMPTY, State, K, fun parseContent/3)
  end;

parseContent(T, #erlsom_sax_state{endtags = EndTags} = State, K) ->
  case EndTags of
    [] ->
      parseEndDocument(T, State, K);
    _ ->
     parseText(T, State, fun(Tail2, State2) ->
     parseContentLT(Tail2, State2, K) end)
  end.

parseEndDocument(Tail, State, K) ->
    %% This is the return value. The second element is what
    %% follows the XML document, as a list.
    State2 = wrapCallback(endDocument, State),
    case State#erlsom_sax_state.is_pull of
        true ->
            %% It is a pull parser, return events in right order
            K({ok, lists:reverse(State2#erlsom_sax_state.user_state), decode(Tail)});
        _ ->
            K({ok, State2#erlsom_sax_state.user_state, decode(Tail)})
    end.


parseText(Tail, #erlsom_sax_state{output = 'utf8'} = State, K) ->
  parseTextBinary(<<>>, Tail, State, K);
parseText(Tail, State, K) ->
  parseText([], Tail, State, K).

parseText(Head, ?STR1_T($<, Tail), State, K) ->
  State2 = wrapCallback({ignorableWhitespace, lists:reverse(Head)}, State),
  K(Tail, State2);
parseText(Head, ?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) -> 
  parseText([NextChar | Head], Tail, State, K);
parseText(Head, ?EMPTY, State, K) ->
  ?CF4K(Head, ?EMPTY, State, K,  fun parseText/4);
parseText(Head, Tail, State, K) ->
  parseTextNoIgnore(Head, Tail, State,K).

parseTextNoIgnore(Head, ?STR1_T($<, Tail), State, K) ->
  State2 = wrapCallback({characters, lists:reverse(Head)}, State),
  K(Tail, State2);
parseTextNoIgnore(Head, ?STR1_T($&, Tail), State, K) ->
  parseReference([], element, Tail, State, fun({Head2, Tail2, State2}) ->
  parseTextNoIgnore(Head2 ++ Head, Tail2, State2, K) end);
parseTextNoIgnore(Head, ?STR1_T(NextChar, Tail), State, K) 
  when NextChar < 16#80 ->
  parseTextNoIgnore([NextChar|Head], Tail, State, K);
parseTextNoIgnore(Head, ?EMPTY, State, K) ->
  ?CF4K(Head, ?EMPTY, State, K, fun parseTextNoIgnore/4);
parseTextNoIgnore(Head, Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  parseTextNoIgnore([Char | Head], Tail2, State2, K) end).


parseTextBinary(Head, ?STR1_T($<, Tail), State, K) ->
  State2 = wrapCallback({ignorableWhitespace, Head}, State),
  K(Tail, State2);
parseTextBinary(Head, ?STR1_T(NextChar, Tail), State, K)
  when ?is_whitespace(NextChar) -> 
  parseTextBinary(<<Head/binary, NextChar>>, Tail, State, K);
parseTextBinary(Head, ?EMPTY, State, K) ->
  ?CF4K(Head, ?EMPTY, State, K, fun parseTextBinary/4);
parseTextBinary(Head, Tail, State, K) ->
  parseTextNoIgnoreBinary(Head, Tail, State, K).

parseTextNoIgnoreBinary(Head, ?STR1_T($<, Tail), State, K) ->
  State2 = wrapCallback({characters, Head}, State),
  K(Tail, State2);
parseTextNoIgnoreBinary(Head, ?STR1_T($&, Tail), State, K) ->
  parseReference([], element, Tail, State, fun({Head2, Tail2, State2}) ->
  %% parseReference returns a list
  Head2Binary = list_to_binary(erlsom_ucs:to_utf8(lists:reverse(Head2))),
  parseTextNoIgnoreBinary(<<Head/binary,  Head2Binary/binary>>, Tail2, State2, K) end);
parseTextNoIgnoreBinary(Head, ?STR1_T(NextChar, Tail), State, K)
  when NextChar < 16#80 ->
  parseTextNoIgnoreBinary(<<Head/binary, NextChar>>, Tail, State, K);
parseTextNoIgnoreBinary(Head, ?EMPTY, State, K) ->
  ?CF4K(Head, ?EMPTY, State, K, fun parseTextNoIgnoreBinary/4);
parseTextNoIgnoreBinary(Head, Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  EncodedChar = erlsom_ucs:char_to_utf8(Char),
  parseTextNoIgnoreBinary(<<Head/binary, EncodedChar/binary>>, Tail2, State2, K) end).

%% entity refernces in attribute values differ fundamentally from
%% refernces in elements and in entity definitions
%% Context can be: element, attribute, definition
parseReference(Head, Context, ?STR1_T($;, Tail), State, K) ->
  K(translateReference(lists:reverse(Head), Context, Tail, State));
parseReference(Head, Context, ?STR1_T(NextChar, Tail), State, K)
  when NextChar < 16#80 ->
  parseReference([NextChar | Head], Context, Tail, State, K);
parseReference(Head, Context, ?EMPTY, State, K) ->
  ?CF5K(Head, Context, ?EMPTY, State, K, fun parseReference/5);
parseReference(Head, Context, Tail, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  parseReference([Char | Head], Context, Tail2, State2, K) end).
  
%% returns: {Head2, Tail2, State2}
%% Character entities are added to the 'head' (the bit that was parsed already),
%% other entities are added to the tail (they still have to be parsed).
%% The problem here is that we have to make sure that we don't get into an infinite 
%% loop. This solved as follows:
%% We proceed by parsing only the entity (while registring in the state that we 
%% are parsing this particular entity). However, we replace the continuation function 
%% by something that simply returns (in stead of calling the function that it was 
%% working on recursively). We then proceed. 
%% Before starting to work on this entity, we need to check that we are not already 
%% parsing this entity (because that would mean an infinite loop).
translateReference(Reference, Context, Tail, State) ->
  %% in the context of a definition, character references have to be replaced
  %% (and others not). 
  case Reference of
    [$#, $x | Tail1] -> %% hex number of char's code point
      %% unfortunately this function accepts illegal values
      %% to do: replace by something that throws an error in case of
      %% an illegal value
      {[httpd_util:hexlist_to_integer(Tail1)], Tail, State};
    [$# | Tail1] -> %% dec number of char's code point
      case catch list_to_integer(Tail1) of
        {'EXIT', _} -> throw({error, "Malformed: Illegal character in reference"});
	%% to do: check on legal character.
	Other -> {[Other], Tail, State}
      end;
    _ -> 
      translateReferenceNonCharacter(Reference, Context, Tail, State)
  end.

translateReferenceNonCharacter(Reference, Context, Tail, 
  State = #erlsom_sax_state{current_entity = CurrentEntity, 
                            max_entity_depth = MaxDepth,
                            entity_relations = Relations,
                            entity_size_acc = TotalSize,
                            max_expanded_entity_size = MaxSize}) ->
  case Context of 
    definition ->
      case MaxDepth of
        0 -> throw({error, "Entities nested too deep"});
        _ -> ok
      end,
      %% check on circular definition
      NewRelation = {CurrentEntity, Reference},
      case lists:member(NewRelation, Relations) of
        true ->
          Relations2 = Relations;
        false ->
          Relations2 = [NewRelation | Relations],
          case erlsom_sax_lib:findCycle(Reference, CurrentEntity, Relations2, MaxDepth) of
            cycle -> 
              throw({error, "Malformed: Cycle in entity definitions"});
            max_depth -> 
              throw({error, "Entities nested too deep"});
            _ -> ok
          end
      end,
      %% don't replace
      {lists:reverse("&" ++ Reference ++ ";"), Tail, State#erlsom_sax_state{entity_relations = Relations2}};
    _ ->
      {Translation, Type} = nowFinalyTranslate(Reference, Context, State),
      NewTotal = TotalSize + length(Translation),
      if
        NewTotal > MaxSize ->
          throw({error, "Too many characters in expanded entities"});
        true ->
          ok
      end,
      case Context of attribute ->
        %% replace, add to the parsed text (head)
        {Translation, Tail, State#erlsom_sax_state{entity_size_acc = NewTotal}};
      _ -> %% element or parameter
        case Type of 
          user_defined ->
            %% replace, encode again and put back into the input stream (Tail)
            TEncoded = encode(Translation),
            {[], combine(TEncoded, Tail), State#erlsom_sax_state{entity_size_acc = NewTotal}};
          _ ->
            {Translation, Tail, State#erlsom_sax_state{entity_size_acc = NewTotal}}
        end
      end
  end.

nowFinalyTranslate(Reference, Context, State) ->
  case Reference of
    "amp" -> {[$&], other};
    "lt" -> {[$<], other};
    "gt" -> {[$>], other};
    "apos" -> {[39], other}; %% apostrof
    "quot" -> {[34], other}; %% quote
  _ -> 
    case State#erlsom_sax_state.expand_entities of
      true -> 
        ListOfEntities = case Context of 
          parameter -> State#erlsom_sax_state.par_entities;
          element -> State#erlsom_sax_state.entities
        end,
        case lists:keysearch(Reference, 1, ListOfEntities) of
          {value, {_, EntityText}} -> 
            {EntityText, user_defined};
          _ ->
            throw({error, "Malformed: unknown reference: " ++ Reference})
        end;
      false ->
        throw({error, "Entity expansion disabled, found reference " ++ Reference})
    end
  end.

%% TODO: proper encoding
-ifdef(BINARY).
combine(Head, Tail) ->
  <<Head/binary, Tail/binary>>.
-endif.

-ifdef(UTF8).
encode(List) ->
  list_to_binary(erlsom_ucs:to_utf8(List)).
-endif.

-ifdef(U16B).
encode(List) ->
  list_to_binary(xmerl_ucs:to_utf16be(List)).
-endif.

-ifdef(U16L).
encode(List) ->
  list_to_binary(xmerl_ucs:to_utf16le(List)).
-endif.

-ifdef(LAT1).
encode(List) ->
  list_to_binary(List).
-endif.

-ifdef(LAT9).
encode(List) ->
  list_to_binary(List).
-endif.

-ifdef(LIST).
encode(List) ->
  List.

combine(Head, Tail) ->
  Head ++ Tail.
-endif.


encodeOutput(List, #erlsom_sax_state{output = 'utf8'}) ->
  list_to_binary(erlsom_ucs:to_utf8(List));
encodeOutput(List, _) ->
  List.

%%parseText(Tail, #erlsom_sax_state{output = 'utf8'} = State) ->
  %%parseTextBinary(?EMPTY, Tail, State);
%%parseText(Tail, State) ->
  %%parseText([], Tail, State).
parseLiteralValue(Tail, Quote, Context, #erlsom_sax_state{output = 'utf8'} = State, K) ->
  parseLiteralValueBinary(Tail, Quote, <<>>, Context, State, K);
parseLiteralValue(Tail, Quote, Context, State, K) ->
  parseLiteralValue(Tail, Quote, [], Context, State, K).

parseLiteralValue(?STR1_T(Quote, Tail), Quote, Head, _Context, State, K) ->
  K(value, lists:reverse(Head), Tail, State);
parseLiteralValue(?STR1_T($&, Tail), Quote, Head, Context, State, K) ->
  parseReference([], Context, Tail, State, fun({Reference, Tail2, State2}) ->
  parseLiteralValue(Tail2, Quote, Reference ++ Head, Context, State2, K) end);
parseLiteralValue(?STR1_T($<, Tail), Quote, Head, Context, State, K) ->
  case Context of
    attribute -> 
      throw({error, "Malformed: < not allowed in literal value"});
    _ -> 
      parseLiteralValue(Tail, Quote, [$< | Head], Context, State, K)
  end;
parseLiteralValue(?STR1_T($%, Tail), Quote, Head, Context, State, K) -> %%
  case Context of
    definition -> 
      %% this is weird, but it follows the implementation of MS 
      %% Internet Explorer...
      %% Can't find this in the standard, but it is an unlikely thing 
      %% to happen in a bonafide xml, and it avoids some problems with 
      %% circular definitions
      throw({error, "Malformed: cannot use % in entity definition (?)"});
    _ -> 
      parseLiteralValue(Tail, Quote, [$% | Head], Context, State, K)
  end;
parseLiteralValue(?STR1_T(NextChar, Tail), Quote, Head, Context, State, K)
  when NextChar < 16#80 ->
  parseLiteralValue(Tail, Quote, [NextChar | Head], Context, State, K);
parseLiteralValue(?EMPTY, Quote, Head, Context, State, K) ->
  ?CF6_2K(?EMPTY, Quote, Head, Context, State, K, fun parseLiteralValue/6);
parseLiteralValue(Tail, Quote, Head, Context, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  parseLiteralValue(Tail2, Quote, [Char | Head], Context, State2, K) end).

parseLiteralValueBinary(?STR1_T(Quote, Tail), Quote, Head, _Context, State, K) ->
  K(value, Head, Tail, State);
parseLiteralValueBinary(?STR1_T($&, Tail), Quote, Head, Context, State, K) ->
  parseReference([], element, Tail, State, fun({Head2, Tail2, State2}) ->
  %% parseReference returns a list (only 1 char long - in case of a 
  %% user defined entity this will be put in front of tail!)
  Head2Binary = list_to_binary(erlsom_ucs:to_utf8(lists:reverse(Head2))),
  parseLiteralValueBinary(Tail2, Quote, <<Head/binary,  Head2Binary/binary>>, Context, State2, K) end);
parseLiteralValueBinary(?STR1_T($<, Tail), Quote, Head, Context, State, K) ->
  case Context of
    attribute -> 
      throw({error, "Malformed: < not allowed in literal value"});
    _ -> 
      parseLiteralValueBinary(Tail, Quote, <<Head/binary, $<>>, Context, State, K)
  end;
parseLiteralValueBinary(?STR1_T($%, Tail), Quote, Head, Context, State, K) -> %%
  case Context of
    definition -> 
      throw({error, "Malformed: cannot use % in entity definition (?)"});
    _ -> 
      parseLiteralValueBinary(Tail, Quote, <<Head/binary, $%>>, Context, State, K)
  end;
parseLiteralValueBinary(?STR1_T(NextChar, Tail), Quote, Head, Context, State, K)
  when NextChar < 16#80 ->
  parseLiteralValueBinary(Tail, Quote, <<Head/binary, NextChar>>, Context, State, K);
parseLiteralValueBinary(?EMPTY, Quote, Head, Context, State, K) ->
  ?CF6_2K(?EMPTY, Quote, Head, Context, State, K, fun parseLiteralValueBinary/6);
parseLiteralValueBinary(Tail, Quote, Head, Context, State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  EncodedChar = erlsom_ucs:char_to_utf8(Char),
  parseLiteralValueBinary(Tail2, Quote, <<Head/binary, EncodedChar/binary>>, Context, State2, K) end).

%% the start tag is decoded (it is a list of unicode code points)
parseEndTag(?STR1_T(A, Tail1), [A | Tail2], State, K)
  when A < 16#80 ->
  parseEndTag(Tail1, Tail2, State, K);
parseEndTag(?STR1_T($>, Tail), [], State, K) ->
  K({ok, Tail, State});
parseEndTag(?STR1_T(NextChar, Tail), [], State, K)
  when ?is_whitespace(NextChar) ->
  removeWS(Tail, State, fun(Tail2, State2) ->
  K({ok, Tail2, State2}) end);
parseEndTag(?EMPTY, StartTag, State, K) ->
  ?CF4_2K(?EMPTY, StartTag, State, K, fun parseEndTag/4);
parseEndTag(Tail, [B | StartTagTail], State, K) ->
  decodeChar(Tail, State, fun(Char, Tail2, State2) ->
  if 
    Char =:= B ->
      parseEndTag(Tail2, StartTagTail, State2, K);
    true -> error
  end end);
parseEndTag(_Tail, [], _State, _K) ->
  error.

removeWS(?STR1_T($>, T), State, K) ->
  K(T, State);
removeWS(?STR1_T(C, T), State, K) 
  when ?is_whitespace(C) ->
  removeWS(T, State, K);
removeWS(?EMPTY, State, K) ->
  ?CF3K(?EMPTY, State, K, fun removeWS/3);
removeWS(_, _, _) ->
  throw({error, "Malformed: Unexpected character in end tag"}).

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
      lookForNamespaces([{LocalName, decodeIfRequired(Value)} | Namespaces], 
                         OtherAttributes, Tail);
    Prefix == [],  LocalName == "xmlns" ->
      lookForNamespaces([{[], decodeIfRequired(Value)} | Namespaces], 
                        OtherAttributes, Tail);
    true -> 
      lookForNamespaces(Namespaces, [Head | OtherAttributes], Tail)
  end;
  
lookForNamespaces(Namespaces, OtherAttributes, []) -> 
  {Namespaces, OtherAttributes}.
 
%decodeIfRequired(URI) -> URI;

decodeIfRequired(URI) when is_list(URI) ->
  URI;
decodeIfRequired(URI) when is_binary(URI) ->
  {Value, _} = erlsom_ucs:from_utf8(URI),
  Value.

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
    Prefix == [] -> {[], LocalName, []};
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

wrapCallback(Event, #erlsom_sax_state{callback = Callback, user_state = UserState} = State) ->
  State#erlsom_sax_state{user_state = Callback(Event, UserState)}.

-ifdef(UTF8).
decode(Bin) ->
  {Value, _} = erlsom_ucs:from_utf8(Bin),
  Value.
-endif.

-ifdef(LAT1).
decode(Bin) ->
  {Value, _} = erlsom_ucs:from_utf8(Bin),
  Value.
-endif.

-ifdef(LAT9).
decode(Bin) ->
  [latin9toUnicode(Char) || Char <- binary_to_list(Bin)].
-endif.

-ifdef(U16B).
decode(Bin) ->
  {Value, _} = erlsom_ucs:from_utf16be(Bin),
  Value.
-endif.

-ifdef(U16L).
decode(Bin) ->
  {Value, _} = erlsom_ucs:from_utf16le(Bin),
  Value.
-endif.

-ifdef(LIST).
decode(List) ->
  List.
-endif.
