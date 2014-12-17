%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2014 6:12
%%%-------------------------------------------------------------------
-module(mekao_core).
-author("tihon").

-include("mekao.hrl").

%% API
-export([qdata/4, build/1, returning/3, where/2, qdata_plain_predicate/4]).


qdata(Num, [], [], _) -> {Num, {[], [], [], []}};
qdata(Num, ['$skip' | Vals], [_Col | Cols], S) -> qdata(Num, Vals, Cols, S);
qdata(Num, [Pred | Vals], [#mekao_column{name = CName} = Col | Cols], S) ->
  {NextNum, NewPH, NewT, NewV} = qdata_predicate(Num, Pred, Col, S),
  {ResNum, {ResCols, ResPHs, ResTypes, ResVals}} = qdata(NextNum, Vals, Cols, S),
  {
    ResNum,
    {[CName | ResCols], [NewPH | ResPHs], [NewT | ResTypes], [NewV | ResVals]}
  }.

build(Q = #mekao_query{body = Select}) when is_record(Select, mekao_select) ->
  #mekao_select{
    columns = Columns,
    table = Table,
    where = Where,
    order_by = OrderBy
  } = Select,

  Q#mekao_query{
    body = [
      mekao_utils:untriplify(Columns, fun(C) -> [<<"SELECT ">>, C] end),
      <<" ">>,
      mekao_utils:untriplify(Table, fun(C) -> [<<"FROM ">>, C] end),
      mekao_utils:untriplify(Where, fun mekao_utils:build_where/1),
      mekao_utils:untriplify(OrderBy, fun mekao_utils:build_order_by/1)
    ]
  };
build(Q = #mekao_query{body = Insert}) when is_record(Insert, mekao_insert) ->
  #mekao_insert{
    table = Table,
    columns = Columns,
    values = Values,
    returning = Return
  } = Insert,

  Q#mekao_query{
    body = [
      <<"INSERT ">>, mekao_utils:untriplify(Table, fun(C) -> [<<"INTO ">>, C] end),
      <<" ">>,
      mekao_utils:untriplify(Columns, fun(Cs) -> [<<"(">>, Cs, <<")">>] end),
      <<" ">>,
      mekao_utils:untriplify(Values, fun(Vs) -> [<<"VALUES ">>, Vs] end),
      mekao_utils:untriplify(Return, fun mekao_utils:build_return/1)
    ]
  };
build(Q = #mekao_query{body = Update}) when is_record(Update, mekao_update) ->
  #mekao_update{
    table = Table,
    set = Set,
    where = Where,
    returning = Return
  } = Update,
  Q#mekao_query{
    body = [
      mekao_utils:untriplify(Table, fun(C) -> [<<"UPDATE ">>, C] end),
      <<" ">>,
      mekao_utils:untriplify(Set, fun(C) -> [<<"SET ">>, C] end),
      mekao_utils:untriplify(Where, fun mekao_utils:build_where/1),
      mekao_utils:untriplify(Return, fun mekao_utils:build_return/1)
    ]
  };
build(Q = #mekao_query{body = Delete}) when is_record(Delete, mekao_delete) ->
  #mekao_delete{
    table = Table,
    where = Where,
    returning = Return
  } = Delete,
  Q#mekao_query{
    body = [
      <<"DELETE ">>,
      mekao_utils:untriplify(Table, fun(C) -> [<<"FROM ">>, C] end),
      mekao_utils:untriplify(Where, fun mekao_utils:build_where/1),
      mekao_utils:untriplify(Return, fun mekao_utils:build_return/1)
    ]
  }.

-spec returning(insert | update | delete, table(), s()) -> iolist().
returning(_QType, _Table, #mekao_settings{returning = undefined}) -> [];
returning(QType, Table, #mekao_settings{returning = RetFun}) ->
  RetFun(QType, Table).

where({[], [], [], []}, _S) -> {[], {[], []}};
where({[C], [PH], [T], [V]}, S) ->
  {W, {NewTs, NewVs}} = mekao_utils:predicate({C, PH, T, V}, S),
  {[W], {NewTs, NewVs}};
where({[C | Cs], [PH | PHs], [T | Types], [V | Vals]}, S) ->
  {W, {NewTs, NewVs}} = mekao_utils:predicate({C, PH, T, V}, S),
  {Ws, {ResTs, ResVs}} = where({Cs, PHs, Types, Vals}, S),
  {[W, <<" AND ">> | Ws], {NewTs ++ ResTs, NewVs ++ ResVs}}.

qdata_plain_predicate(Num, V, Col, S) ->
  #mekao_settings{placeholder = PHFun} = S,
  #mekao_column{type = T, transform = TrFun} = Col,
  TransV = mekao_utils:transform(TrFun, V),
  PH = PHFun(Col, Num, TransV),
  {Num + 1, PH, T, TransV}.


%% @private
qdata_predicate(Num, {'$predicate', 'not', Pred}, Col, S) ->
  {NextNum, NewPH, NewT, NewV} = qdata_predicate(Num, Pred, Col, S),
  {NextNum, NewPH, NewT, {'$predicate', 'not', NewV}};
qdata_predicate(Num, {'$predicate', in, InVals}, Col, S) ->
  #mekao_settings{placeholder = PHFun} = S,
  #mekao_column{type = T, transform = TrFun} = Col,
  %% intentional `error:badmatch' to prevent empty `... IN ()'
  true = is_list(InVals) andalso InVals /= [],
  {NewNum, RevPHs, RevTypes, RevVals} =
    lists:foldl(
      fun(InV, {InNum, InPHs, InTypes, InTransVals}) ->
        TransV = mekao_utils:transform(TrFun, InV),
        PH = PHFun(Col, InNum, TransV),
        {InNum + 1, [PH | InPHs], [T | InTypes],
          [TransV | InTransVals]}
      end, {Num, [], [], []}, InVals
    ),
  {NewNum, lists:reverse(RevPHs), lists:reverse(RevTypes),
    {'$predicate', in, lists:reverse(RevVals)}};
qdata_predicate(Num, {'$predicate', 'between', V1, V2}, Col, S) ->
  #mekao_settings{placeholder = PHFun} = S,
  #mekao_column{type = T, transform = TrFun} = Col,
  TransV1 = mekao_utils:transform(TrFun, V1),
  TransV2 = mekao_utils:transform(TrFun, V2),
  PH1 = PHFun(Col, Num, TransV1),
  PH2 = PHFun(Col, Num + 1, TransV2),
  {Num + 2, {PH1, PH2}, T,
    {'$predicate', 'between', TransV1, TransV2}
  };
qdata_predicate(Num, {'$predicate', Op, V}, Col, S) ->
  #mekao_settings{placeholder = PHFun} = S,
  #mekao_column{type = T, transform = TrFun} = Col,
  TransV = mekao_utils:transform(TrFun, V),
  PH = PHFun(Col, Num, TransV),
  {Num + 1, PH, T, {'$predicate', Op, TransV}};
qdata_predicate(Num, V, Col, S) ->
  qdata_plain_predicate(Num, V, Col, S).