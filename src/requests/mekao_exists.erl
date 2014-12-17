%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2014 6:04
%%%-------------------------------------------------------------------
-module(mekao_exists).
-author("tihon").

-include("mekao.hrl").

%% API
-export([exists/3, prepare_exists/3, exists_pk/3]).


exists(E, Table, S) ->
  {ok, mekao_core:build(prepare_exists(E, Table, S))}.

exists_pk(E, Table, S) ->
  Q = prepare_exists(
    mekao_utils:skip(fun mekao_utils:skip_not_pk/2, Table#mekao_table.columns, mekao_utils:e2l(E)),
    Table, S
  ),
  if (Q#mekao_query.body)#mekao_select.where /= [] ->
    {ok, mekao_core:build(Q)};
    true ->
      {error, pk_miss}
  end.

prepare_exists(E, Table, S) ->
  PrepQ = #mekao_query{
    body = PrepQBody = #mekao_select{order_by = OrderBy}
  } = mekao_select:prepare_select(mekao_utils:e2l(E), Table, S),
  PrepQ#mekao_query{
    body = PrepQBody#mekao_select{
      columns = {
        <<"SELECT COUNT(*) AS exists"
        " FROM (SELECT 1) AS t WHERE EXISTS(">>,
        <<"1">>, <<"">>
      },
      order_by = {<<"">>, OrderBy, <<")">>}
    }
  }.