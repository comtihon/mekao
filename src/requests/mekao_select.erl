%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2014 5:11
%%%-------------------------------------------------------------------
-module(mekao_select).
-author("tihon").

-include("mekao.hrl").

%% API
-export([select_pk/3, select/3, select/4, prepare_select/3, prepare_select/4]).


select_pk(E, Table, S) ->
  Q = prepare_select(
    mekao_utils:skip(fun mekao_utils:skip_not_pk/2, Table#mekao_table.columns, mekao_utils:e2l(E)),
    Table, S
  ),
  if (Q#mekao_query.body)#mekao_select.where /= [] ->
    {ok, mekao_core:build(Q)};
    true ->
      {error, pk_miss}
  end.

select(E, Table, S) ->
  select(E, [], Table, S).

select(E, Opts, Table, S) ->
  {ok, mekao_core:build(prepare_select(E, Opts, Table, S))}.

prepare_select(E, Table, S) ->
  prepare_select(E, [], Table, S).

prepare_select(E, Opts, Table, S) ->
  #mekao_table{
    columns = MekaoCols,
    order_by = OrderBy
  } = Table,

  {NextNum, QData} = mekao_core:qdata(1, mekao_utils:e2l(E), MekaoCols, S),
  {Where, {Types, Vals}} = mekao_core:where(QData, S),

  AllCols = mekao_utils:intersperse(
    MekaoCols, <<", ">>, fun(#mekao_column{name = Name}) -> Name end
  ),

  Q = #mekao_select{
    table = Table#mekao_table.name,
    columns = AllCols,
    where = Where,
    order_by = order_by(OrderBy)
  },
  limit(
    #mekao_query{
      body = Q,
      types = Types,
      values = Vals,
      next_ph_num = NextNum
    }, Opts, S
  ).


%% @private
limit(PSelect, Opts, #mekao_settings{limit = undefined}) ->
  %% intentional error:badmatch in case if `limit' was specified but
  %% `#mekao_settings.limit' was not
  undefined = proplists:get_value(limit, Opts),
  PSelect;
limit(PSelect, Opts, #mekao_settings{limit = LimitFun}
) when is_function(LimitFun, 3) ->
  case proplists:get_value(limit, Opts) of
    undefined ->
      PSelect;
    {RowCount, Offset} ->
      #mekao_query{} = LimitFun(PSelect, RowCount, Offset)
  end.

%% @private
order_by([]) -> [];
order_by([O]) -> [order_by_1(O)];
order_by([O | OrderBy]) -> [order_by_1(O), <<", ">> | order_by(OrderBy)].

%% @private
order_by_1(E) when not is_tuple(E) -> order_by_1({E, {default, default}});
order_by_1({Pos, Opts}) when is_integer(Pos) -> order_by_1({integer_to_list(Pos - 1), Opts});
order_by_1({Expr, Opts}) when is_list(Expr); is_binary(Expr) -> [Expr, order_by_opts(Opts)].

%% @private
order_by_opts({Ordering, Nulls}) ->
  O = case Ordering of
        default -> <<"">>;
        asc -> <<" ASC">>;
        desc -> <<" DESC">>
      end,
  case Nulls of
    default -> O;
    nulls_first -> <<O/binary, " NULLS FIRST">>;
    nulls_last -> <<O/binary, " NULLS LAST">>
  end.