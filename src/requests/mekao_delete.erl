%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2014 5:11
%%%-------------------------------------------------------------------
-module(mekao_delete).
-author("tihon").

-include("mekao.hrl").

%% API
-export([delete/3, delete_pk/3, prepare_delete/3]).


delete_pk(E, Table, S) ->
  Q = prepare_delete(
    mekao_utils:skip(fun mekao_utils:skip_not_pk/2, Table#mekao_table.columns, mekao_utils:e2l(E)),
    Table, S
  ),
  if (Q#mekao_query.body)#mekao_delete.where /= [] ->
    {ok, mekao_core:build(Q)};
    true ->
      {error, pk_miss}
  end.

delete(Selector, Table, S) ->
  {ok, mekao_core:build(prepare_delete(Selector, Table, S))}.

prepare_delete(E, Table, S) ->
  {NextNum, QData} =
    mekao_core:qdata(1, mekao_utils:e2l(E), Table#mekao_table.columns, S),
  {Where, {Types, Vals}}
    = mekao_core:where(QData, S),

  Q = #mekao_delete{
    table       = Table#mekao_table.name,
    where       = Where,
    returning   = mekao_core:returning(delete, Table, S)
  },
  #mekao_query{
    body     = Q,
    types    = Types,
    values   = Vals,
    next_ph_num = NextNum
  }.