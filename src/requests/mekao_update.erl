%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2014 5:11
%%%-------------------------------------------------------------------
-module(mekao_update).
-author("tihon").

-include("mekao.hrl").

%% API
-export([update_pk/3, update_pk_diff/4, update/4]).


update_pk(E, Table = #mekao_table{columns = MekaoCols}, S) ->
  SetSkipFun =
    fun(#mekao_column{ro = RO, key = Key}, V) ->
      RO orelse V == '$skip' orelse Key
    end,

  Vals = mekao_utils:e2l(E),
  Q = prepare_update(
    mekao_utils:skip(SetSkipFun, MekaoCols, Vals),
    mekao_utils:skip(fun mekao_utils:skip_not_pk/2, MekaoCols, Vals),
    Table, S
  ),
  if (Q#mekao_query.body)#mekao_update.set == [] ->
    {error, empty_update};
    (Q#mekao_query.body)#mekao_update.where == [] ->
      {error, pk_miss};
    true ->
      {ok, mekao_core:build(Q)}
  end.

update_pk_diff(E1, E2, Table = #mekao_table{columns = MekaoCols}, S) ->
  Vals1 = mekao_utils:e2l(E1),
  Vals2 = mekao_utils:e2l(E2),
  DiffVals = mekao_utils:map2(
    fun
      (V, V) -> '$skip';
      (_, V2) -> V2
    end, Vals1, Vals2
  ),

  Q = prepare_update(
    mekao_utils:skip(fun mekao_utils:skip_ro_or_skip/2, MekaoCols, DiffVals),
    mekao_utils:skip(fun mekao_utils:skip_not_pk/2, MekaoCols, Vals1),
    Table, S
  ),

  if (Q#mekao_query.body)#mekao_update.set == [] ->
    {error, empty_update};
    (Q#mekao_query.body)#mekao_update.where == [] ->
      {error, pk_miss};
    true ->
      {ok, mekao_core:build(Q)}
  end.

update(E, Selector, Table = #mekao_table{columns = MekaoCols}, S) ->
  Q = prepare_update(
    mekao_utils:skip(fun mekao_utils:skip_ro_or_skip/2, MekaoCols, mekao_utils:e2l(E)),
    Selector, Table, S
  ),
  if (Q#mekao_query.body)#mekao_update.set == [] ->
    {error, empty_update};
    true ->
      {ok, mekao_core:build(Q)}
  end.

prepare_update(SetE, WhereE, Table = #mekao_table{columns = MekaoCols}, S) ->
  {SetNextNum, {SetCols, SetPHs, SetTypes, SetVals}} =
    mekao_core:qdata(1, mekao_utils:e2l(SetE), MekaoCols, S),

  {WhereNextNum, WhereQData} =
    mekao_core:qdata(SetNextNum, mekao_utils:e2l(WhereE), MekaoCols, S),

  {Where, {WhereTypes, WhereVals}} =
    mekao_core:where(WhereQData, S),

  Set = mekao_utils:intersperse2(
    fun (C, PH) -> [C, <<" = ">>, PH] end,
    <<", ">>, SetCols, SetPHs
  ),

  Q = #mekao_update{
    table       = Table#mekao_table.name,
    set         = Set,
    where       = Where,
    returning   = mekao_core:returning(update, Table, S)
  },
  #mekao_query{
    body     = Q,
    types    = SetTypes ++ WhereTypes,
    values   = SetVals ++ WhereVals,
    next_ph_num = WhereNextNum
  }.