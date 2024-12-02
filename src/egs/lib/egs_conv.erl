%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 转换模块
%%% @end
%%%-------------------------------------------------------------------
-module(egs_conv).

%%
%% TODO 此模块是N年前实现，当前版本可以考虑优化更新
%%

-export([
  int_to_hex/1
  , list_to_hex/1
  , to_md5/1
  , to_list/1
  , to_tuple/1
  , to_integer/1
  , to_float/1
  , to_float/2
  , to_bool/1
  , to_binary/1
  , to_atom/1
  , to_iodata/1
  , to_hex/1
  , to_unicode/1
  , term_to_bitstring/1
  , term_to_bitstring2/1
  , term_to_string/1
  , term_to_string2/1
  , string_to_term/1
  , bitstring_to_term/1
  , is_string/1
]).

%%
int_to_hex(N) when N < 256 -> [to_hex(N div 16), to_hex(N rem 16)].

to_hex(N) when is_integer(N), N < 10 ->
  $0 + N;
to_hex(N) when is_integer(N), N >= 10, N < 16 ->
  $a + (N - 10);
to_hex(N) -> N.

%%LIST转HEX
list_to_hex(L) -> lists:map(fun(X) -> int_to_hex(X) end, L).

%%MD5
to_md5(S) ->
  Md5_bin = erlang:md5(S),
  Md5_list = binary_to_list(Md5_bin),
  lists:flatten(list_to_hex(Md5_list)).

%%转换LIST
to_list(Msg) when is_list(Msg) -> Msg;
to_list(Msg) when is_atom(Msg) -> atom_to_list(Msg);
to_list(Msg) when is_binary(Msg) -> binary_to_list(Msg);
to_list(Msg) when is_integer(Msg) -> integer_to_list(Msg);
to_list(Msg) when is_float(Msg) -> float_to_list(Msg);
to_list(Msg) when is_tuple(Msg) -> tuple_to_list(Msg);
to_list(Msg) when is_map(Msg) -> maps:to_list(Msg);
to_list(_) -> throw(badarg).


%%转Integer
to_integer(Msg) when is_integer(Msg) -> Msg;
to_integer(Msg) when is_binary(Msg) -> binary_to_integer(Msg);
to_integer(Msg) when is_list(Msg) -> list_to_integer(Msg);
to_integer(Msg) when is_float(Msg) -> round(Msg);
to_integer(_Msg) -> throw(badarg).

%%转FLOAT
to_float(Msg) when is_float(Msg) -> Msg;
to_float(Msg) when is_binary(Msg) -> binary_to_float(Msg);
to_float(Msg) when is_list(Msg) -> list_to_float(Msg);
to_float(Msg) when is_integer(Msg) -> Msg / 1.0;
to_float(_Msg) -> throw(badarg).

%%保留N位小数
to_float(Value, Decimal) when is_float(Value) andalso is_integer(Decimal) ->
  binary_to_float(float_to_binary(Value, [{decimals, Decimal}, compact]));
to_float(Value, _Decimal) ->
  to_float(Value).

%%转BOOL
to_bool(D) when is_boolean(D) -> D;
to_bool(D) when is_number(D) -> D /= 0;
to_bool("false") -> false;
to_bool("true") -> true;
to_bool(D) when is_binary(D) -> to_bool(string:to_lower(to_list(D)));
to_bool(D) when is_atom(D) ->
  D == true;
to_bool(_D) -> throw(badarg).


%%转TUPLE
to_tuple(T) when is_tuple(T) -> T;
to_tuple(T) when is_list(T) -> list_to_tuple(T);
to_tuple(T) when is_binary(T) -> binary_to_term(T);
to_tuple(T) when is_map(T) -> to_tuple(maps:to_list(T));
to_tuple(T) when is_number(T) -> {T};
to_tuple(_) -> throw(badarg).


%%转BIN
to_binary(Msg) when is_binary(Msg) -> Msg;
to_binary(Msg) when is_atom(Msg) -> atom_to_binary(Msg, utf8);
to_binary(Msg) when is_list(Msg) -> list_to_binary(Msg);
to_binary(Msg) when is_integer(Msg) -> integer_to_binary(Msg);
to_binary(Msg) when is_float(Msg) -> float_to_binary(Msg);
to_binary(Msg) -> term_to_binary(Msg).

%% 转成iodata
to_iodata(Any) when is_list(Any) ->
  Any;
to_iodata(Any) when is_binary(Any) ->
  Any;
to_iodata(Any) when is_atom(Any) ->
  to_list(Any);
to_iodata(Any) when is_integer(Any) ->
  to_list(Any);
to_iodata(Any) when is_float(Any) ->
  to_list(Any).


%%转ATOM
to_atom(Msg) when is_atom(Msg) -> Msg;
to_atom(Msg) when is_binary(Msg) ->
  case catch binary_to_existing_atom(Msg, utf8) of
    Atom when is_atom(Atom) ->
      Atom;
    _ ->
      binary_to_atom(Msg, utf8)
  end;
to_atom(Msg) when is_list(Msg) ->
  case catch (list_to_existing_atom(Msg)) of
    Atom when is_atom(Atom) -> Atom;
    _ ->
      list_to_atom(Msg)
  end;
to_atom(_) -> throw(badarg).


%%bitstring2term
bitstring_to_term(undefined) -> [];
bitstring_to_term(<<>>) -> [];
bitstring_to_term(<<131, _, _/binary>> = B) -> binary_to_term(B);
bitstring_to_term(BitString) when is_binary(BitString) -> string_to_term(binary_to_list(BitString));
bitstring_to_term(BitString) ->
  string_to_term(BitString).

%%term2string
term_to_string(Term) -> io_lib:format("~w", [Term]).
term_to_bitstring(Term) -> list_to_bitstring(term_to_string(Term)).

term_to_string2(Term) -> io_lib:format("~p", [Term]).
term_to_bitstring2(Term) -> list_to_bitstring(term_to_string2(Term)).


%%string2term
string_to_term(String) when is_binary(String) ->
  string_to_term(to_binary(String));
string_to_term(String) ->
  case erl_scan:string(String ++ ".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_term(Tokens) of
        {ok, L} -> L;
        _ -> []
      end;
    {error, Err, _} ->
      throw(Err);
    Err ->
      throw(Err)
  end.

%%是否string
is_string([]) -> true;
is_string(List) -> is_string(List, non_unicode).
is_string([C | Rest], non_unicode) when C >= 0, C =< 255 -> is_string(Rest, non_unicode);
is_string([C | Rest], _) when C =< 65000 -> is_string(Rest, unicode);
is_string([], non_unicode) -> true;
is_string([], unicode) -> true;
is_string(_, _) -> false.

%%转unicode
to_unicode(B) when is_integer(B) -> <<B/utf8>>;
to_unicode(B) when is_binary(B) -> B;
to_unicode(B) when is_list(B) ->
  unicode:characters_to_binary(B, utf8);
to_unicode(B) ->
  B1 = iolist_to_binary(B),
  <<B1/utf8>>.