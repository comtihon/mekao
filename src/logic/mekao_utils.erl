-module(mekao_utils).

%% API
-export([
  identity/1,
  is_null/1,
  intersperse/2,
  intersperse/3,
  intersperse2/4,
  map2/3,
  map3/4,
  skip/3,
  e2l/1,
  skip_not_pk/2,
  skip_ro_or_skip/2,
  untriplify/2,
  transform/2,
  build_where/1,
  predicate/2,
  build_return/1,
  build_order_by/1]).

-include("mekao.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec map2(fun((V1 :: term(), V2 :: term()) -> ResV :: term()),
    L1 :: list(), L2 :: list()) -> list().
map2(_Fun, [], []) -> [];
map2(Fun, [V1 | L1], [V2 | L2]) -> [Fun(V1, V2) | map2(Fun, L1, L2)].


-spec map3(fun((V1 :: term(), V2 :: term(), V3 :: term()) -> ResV :: term()),
    L1 :: list(), L2 :: list(), L3 :: list()) -> list().
map3(_Fun, [], [], []) -> [];
map3(Fun, [V1 | L1], [V2 | L2], [V3 | L3]) ->
  [Fun(V1, V2, V3) | map3(Fun, L1, L2, L3)].


-spec is_null(term()) -> boolean().
is_null(V) -> V == undefined.

-spec identity(term()) -> term().
identity(X) -> X.

intersperse(List, Sep) ->
  intersperse(List, Sep, fun identity/1).

-spec intersperse(List :: list(), Separator :: term(), ValFun :: fun((term()) -> term())) -> list().
intersperse([], _, _) -> [];
intersperse([Item], _, Fun) -> [Fun(Item)];
intersperse([Item | Items], Sep, Fun) ->
  [Fun(Item), Sep | intersperse(Items, Sep, Fun)].
intersperse2(_Fun, _Sep, [], []) -> [];
intersperse2(Fun, _Sep, [I1], [I2]) -> [Fun(I1, I2)];
intersperse2(Fun, Sep, [I1 | I1s], [I2 | I2s]) ->
  [Fun(I1, I2), Sep | intersperse2(Fun, Sep, I1s, I2s)].

skip(SkipFun, Cols, Vals) ->
  mekao_utils:map2(
    fun(C, V) ->
      Skip = SkipFun(C, V),
      if Skip -> '$skip';
        true -> V
      end
    end, Cols, Vals
  ).

%% @doc entity to list
e2l(Vals) when is_list(Vals) -> Vals;
e2l(E) when is_tuple(E) ->
  [_EntityName | Vals] = tuple_to_list(E),
  Vals.

skip_not_pk(#mekao_column{key = Key}, _) -> not Key.
skip_ro_or_skip(#mekao_column{ro = RO}, V) -> RO orelse V == '$skip'.

untriplify({C1, C2, C3}, F) when is_function(F) -> [C1, F(C2), C3];
untriplify(C, F) -> F(C).

transform(undefined, V) -> V;
transform(TrFun, V) when is_function(TrFun, 1) -> TrFun(V).

build_where([]) -> <<>>;
build_where(Where) -> [<<" WHERE ">> | Where].

build_return([]) -> <<>>;
build_return(Return) -> [<<" ">> | Return].

build_order_by([]) -> <<>>;
build_order_by(OrderBy) -> [<<" ORDER BY ">>, OrderBy].

%% TODO: add ANY, ALL handling
predicate({C, PH, T, {'$predicate', Op, V}}, S) when Op == '='; Op == '<>' ->
  IsNull = (S#mekao_settings.is_null)(V),
  if not IsNull ->
    {[C, op_to_bin(Op), PH], {[T], [V]}};
    Op == '=' ->
      {[C, <<" IS NULL">>], {[], []}};
    Op == '<>' ->
      {[C, <<" IS NOT NULL">>], {[], []}}
  end;
predicate({C, PH, T, {'$predicate', 'not', Pred}}, _S) ->
  {W, Rest} = predicate({C, PH, T, Pred}, _S),
  {[<<"NOT (">>, W, <<")">>], Rest};
predicate({C, {PH1, PH2}, T, {'$predicate', between, V1, V2}}, _S) ->
  {[C, <<" BETWEEN ">>, PH1, <<" AND ">>, PH2], {[T, T], [V1, V2]}};
predicate({C, PH, T, {'$predicate', like, V}}, _S) ->
  {[C, <<" LIKE ">>, PH], {[T], [V]}};
predicate({C, PHs, Ts, {'$predicate', in, Vals}}, _S) when is_list(Vals), Vals /= [] ->
  {[C, <<" IN (">>,
    mekao_utils:intersperse(PHs, <<", ">>),
    <<")">>
  ], {Ts, Vals}};
predicate({C, PH, T, {'$predicate', OP, V}}, _S) ->
  {[C, op_to_bin(OP), PH], {[T], [V]}};
predicate({C, PH, T, V}, S) ->
  predicate({C, PH, T, {'$predicate', '=', V}}, S).


%% @private
op_to_bin('=') -> <<" = ">>;
op_to_bin('<>') -> <<" <> ">>;
op_to_bin('>') -> <<" > ">>;
op_to_bin('>=') -> <<" >= ">>;
op_to_bin('<') -> <<" < ">>;
op_to_bin('<=') -> <<" <= ">>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

map2_test_() ->
  F = fun(A, B) -> A + B end,
  [
    ?_assertMatch([], map2(F, [], [])),
    ?_assertMatch([3], map2(F, [1], [2])),
    ?_assertMatch([10, 10, 10], map2(F, [1, 2, 3], [9, 8, 7])),
    ?_assertException(error, function_clause, map2(F, [1], [])),
    ?_assertException(error, function_clause, map2(F, [], [1]))
  ].

map3_test_() ->
  F = fun(A, B, C) -> A + B + C end,
  [
    ?_assertMatch([], map3(F, [], [], [])),
    ?_assertMatch([6], map3(F, [1], [2], [3])),
    ?_assertMatch([15, 15, 15], map3(F, [1, 2, 3], [9, 8, 7], [5, 5, 5])),
    ?_assertException(error, function_clause, map3(F, [1], [], [])),
    ?_assertException(error, function_clause, map3(F, [], [1], [])),
    ?_assertException(error, function_clause, map3(F, [], [], [1]))
  ].

intersperse_test_() ->
  L = [a, b, c],
  ValFun = fun(a) -> 1; (b) -> 2; (c) -> 3 end,

  [
    ?_assert([1, x, 2, x, 3] == intersperse(L, x, ValFun)),
    ?_assert([1] == intersperse([a], x, ValFun)),
    ?_assert([] == intersperse([], x, ValFun))
  ].

intersperse2_test_() ->
  ValFun = fun(V1, V2) -> {V1, V2} end,

  [
    ?_assert(
      intersperse2(ValFun, x, [a, b, c, d], [d, c, b, a])
        == [{a, d}, x, {b, c}, x, {c, b}, x, {d, a}]
    ),
    ?_assert(intersperse2(ValFun, x, [a], [b]) == [{a, b}]),
    ?_assert(intersperse2(ValFun, x, [], []) == [])
  ].

-endif.
