%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2014 5:11
%%%-------------------------------------------------------------------
-module(mekao_insert).
-author("tihon").

-include("mekao.hrl").

%% API
-export([insert/3, insert_all/3, prepare_insert/3, prepare_insert_all/3]).


insert(E, Table, S) ->
  Q = prepare_insert(
    mekao_utils:skip(fun mekao_utils:skip_ro_or_skip/2, Table#mekao_table.columns, mekao_utils:e2l(E)),
    Table, S
  ),

  if Q#mekao_query.values /= [] ->
    {ok, mekao_core:build(Q)};
    true ->
      {error, empty_insert}
  end.

insert_all(Es = [_ | _], Table, S) ->
  Q = prepare_insert_all(
    [mekao_utils:skip(fun mekao_utils:skip_ro_or_skip/2, Table#mekao_table.columns, mekao_utils:e2l(E))
      || E <- Es],
    Table, S
  ),
  {ok, mekao_core:build(Q)}.

prepare_insert(E, Table, S) ->
  {NextNum, {Cols, PHs, Types, Vals}} =
    mekao_core:qdata(1, mekao_utils:e2l(E), Table#mekao_table.columns, S),
  Q = #mekao_insert{
    table   = Table#mekao_table.name,
    columns = mekao_utils:intersperse(Cols, <<", ">>),
    values  = [<<"(">>, mekao_utils:intersperse(PHs, <<", ">>), <<")">>],
    returning = mekao_core:returning(insert, Table, S)
  },
  #mekao_query{
    body    = Q,
    types   = Types,
    values  = Vals,
    next_ph_num = NextNum
  }.

prepare_insert_all(Es = [_ | _], Table, S) ->
  MekaoCols = Table#mekao_table.columns,
  {ResNum, {RevPHs, RevTypes, RevVals}} =
    lists:foldl(
      fun(E, {AccNum, {AccPHs, AccTypes, AccVals}}) ->
        {NextNum, {PHs, Types, Vals}} =
          qdata_insert(AccNum, mekao_utils:e2l(E), MekaoCols, S),
        {NextNum, {
          [PHs | AccPHs],
            lists:reverse(Types) ++ AccTypes,
            lists:reverse(Vals) ++ AccVals
        }}
      end, {1, {[], [], []}}, Es
    ),

  Q = #mekao_insert{
    table = Table#mekao_table.name,
    columns =
    mekao_utils:intersperse(
      MekaoCols, <<", ">>,
      fun(#mekao_column{name = Name}) -> Name end
    ),
    values =
    mekao_utils:intersperse(
      lists:reverse(RevPHs), <<", ">>,
      fun(PHs) ->
        [<<"(">>, mekao_utils:intersperse(PHs, <<", ">>), <<")">>]
      end
    ),
    returning = mekao_core:returning(insert, Table, S)
  },
  #mekao_query{
    body     = Q,
    types    = lists:reverse(RevTypes),
    values   = lists:reverse(RevVals),
    next_ph_num = ResNum
  }.


%% @private
qdata_insert(Num, [], [], _) ->
  {Num, {[], [], []}};

qdata_insert(Num, ['$skip' | Vals], [_Col | Cols], S) ->
  {ResNum, {ResPHs, ResTypes, ResVals}} = qdata_insert(
    Num, Vals, Cols, S
  ),
  {ResNum, {[<<"DEFAULT">> | ResPHs], ResTypes, ResVals}};

qdata_insert(Num, [Pred | Vals], [Col | Cols], S) ->
  {NextNum, NewPH, NewT, NewV} = mekao_core:qdata_plain_predicate(Num, Pred, Col, S),

  {ResNum, {ResPHs, ResTypes, ResVals}} = qdata_insert(
    NextNum, Vals, Cols, S
  ),

  {ResNum, {[NewPH | ResPHs], [NewT | ResTypes], [NewV | ResVals]}}.

