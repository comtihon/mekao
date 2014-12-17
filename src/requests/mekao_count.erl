%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Дек. 2014 6:04
%%%-------------------------------------------------------------------
-module(mekao_count).
-author("tihon").

-include("mekao.hrl").

%% API
-export([count/3, prepare_count/3]).


count(E, Table, S) ->
  {ok, mekao_core:build(prepare_count(E, Table, S))}.

prepare_count(E, Table, S) ->
  PrepQ = #mekao_query{body = PrepQBody} =
    mekao_select:prepare_select(mekao_utils:e2l(E), Table, S),
  PrepQ#mekao_query{
    body = PrepQBody#mekao_select{
      columns = <<"COUNT(*) as count">>
    }
  }.
