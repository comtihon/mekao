-module(mekao).

%% API
-export([
  select_pk/3, select/3, select/4,
  exists_pk/3, exists/3,
  count/3,
  insert/3, insert_all/3,
  update_pk/3,
  update_pk_diff/4,
  update/4,
  delete_pk/3,
  delete/3,

  prepare_select/3, prepare_select/4,
  prepare_exists/3, prepare_count/3,
  prepare_insert/3, prepare_insert_all/3,
  prepare_update/4,
  prepare_delete/3,
  build/1
]).

-include("mekao.hrl").

-type iotriple() :: iodata() | {iodata(), iodata(), iodata()}.

-type table() :: #mekao_table{}.
-type column() :: #mekao_column{}.
-type s() :: #mekao_settings{}.

-type entity() :: tuple() | list(term() | '$skip').

-type predicate() :: term()
| {'$predicate', between, term(), term()}
| {'$predicate', in, [term(), ...]}
| {'$predicate', '=' | '<>' | '>' | '>=' | '<' | '<=' | like, term()}
| {'$predicate', 'not', predicate()}.

-type select_opt() :: {limit, {RowCount :: non_neg_integer(), Offset :: non_neg_integer()}}.

%% generic query
-type 'query'(Body) :: #mekao_query{body :: Body}.

%% prepared query
-type p_query() :: 'query'(#mekao_insert{} | #mekao_select{} | #mekao_update{} | #mekao_delete{}).
%% built query
-type b_query() :: 'query'(iolist()).

-export_type([
  iotriple/0,
  table/0, column/0, s/0,
  'query'/1, p_query/0, b_query/0,
  predicate/0
]).

%% ===================================================================
%% API functions
%% ===================================================================
-spec select_pk(selector(), table(), s()) -> {ok, b_query()}
| {error, pk_miss}.
%% @doc Reads entity by it's primary key.
select_pk(A, B, C) -> mekao_select:select_pk(A, B, C).

-spec select(selector(), table(), s()) -> {ok, b_query()}.
%% @doc Selects several entities, omits columns with `$skip' value.
select(A, B, C) -> mekao_select:select(A, B, C).

-spec select(selector(), [select_opt()], table(), s()) -> {ok, b_query()}.
select(A, B, C, D) -> mekao_select:select(A, B, C, D).

-spec exists_pk(selector(), table(), s()) -> {ok, b_query()}
| {error, pk_miss}.
%% @doc Same as `exists/3' but checks only primary key in `EXISTS' clause.
exists_pk(A, B, C) -> mekao_exists:exists_pk(A, B, C).

-spec exists(selector(), table(), s()) -> {ok, b_query()}.
%% @doc Selects one column called `exists' with value `1' or `0', depends on
%%      `EXISTS' clause return.
exists(A, B, C) -> mekao_exists:exists(A, B, C).

-spec count(selector(), table(), s()) -> {ok, b_query()}.
%% @doc Selects `COUNT(*)' with `WHERE' clause based on `selector()'.
count(A, B, C) -> mekao_count:count(A, B, C).

-spec insert(entity(), table(), s()
) -> {ok, b_query()} | {error, empty_insert}.
%% @doc Inserts entity, omits columns with `$skip' value.
insert(A, B, C) -> mekao_insert:insert(A, B, C).

-spec insert_all([entity(), ...], table(), s()) -> {ok, b_query()}.
%% @doc Inserts entities, places `DEFAULT' keyword when column with `$skip'
%%      value occurs.
insert_all(A, B, C) -> mekao_insert:insert_all(A, B, C).

-spec update_pk(selector(), table(), s()) -> {ok, b_query()}
| {error, pk_miss}
| {error, empty_update}.
%% @doc Updates entity by it's primary key, omits columns with `$skip' value.
update_pk(A, B, C) -> mekao_update:update_pk(A, B, C).

-spec update_pk_diff(Old :: entity(), New :: entity(), table(), s()
) -> {ok, b_query()}
| {error, pk_miss}
| {error, empty_update}.
%% @doc Updates only changed fields by primary key. This is possible to update
%%      PK as well if it is not `ro = true'.
update_pk_diff(A, B, C, D) -> mekao_update:update_pk_diff(A, B, C, D).

-spec update(entity(), selector(), table(), s()) -> {ok, b_query()}
| {error, empty_update}.
%% @doc Updates entities, composes WHERE clause from `Selector'
%%      non `$skip' fields. This is possible to update PK as well if it
%%      is not `ro = true'.
update(A, B, C, D) -> mekao_update:update(A, B, C, D).

-spec delete_pk(selector(), table(), s()) -> {ok, b_query()} | {error, pk_miss}.
%% @doc Deletes entity by primary key.
delete_pk(A, B, C) -> mekao_delete:delete_pk(A, B, C).

-spec delete(selector(), table(), s()) -> {ok, b_query()}.
%% @doc Deletes entities, composes WHERE clause  from `Selector'
%%      non `$skip' fields.
delete(A, B, C) -> mekao_delete:delete(A, B, C).

-spec prepare_select(selector(), table(), s()) -> p_query().
prepare_select(A, B, C) -> mekao_select:prepare_select(A, B, C).

-spec prepare_select(selector(), [select_opt()], table(), s()) -> p_query().
prepare_select(A, B, C, D) -> mekao_select:prepare_select(A, B, C, D).

-spec prepare_exists(selector(), table(), s()) -> p_query().
prepare_exists(A, B, C) -> mekao_exists:prepare_exists(A, B, C).

-spec prepare_count(selector(), table(), s()) -> p_query().
prepare_count(A, B, C) -> mekao_count:prepare_count(A, B, C).

-spec prepare_insert(entity(), table(), s()) -> p_query().
prepare_insert(A, B, C) -> mekao_insert:prepare_insert(A, B, C).

-spec prepare_insert_all([entity(), ...], table(), s()) -> p_query().
prepare_insert_all(A, B, C) -> mekao_insert:prepare_insert_all(A, B, C).

-spec prepare_update(entity(), selector(), table(), s()) -> p_query().
prepare_update(A, B, C, D) -> mekao_update:prepare_update(A, B, C, D).

-spec prepare_delete(selector(), table(), s()) -> p_query().
prepare_delete(A, B, C) -> mekao_delete:prepare_delete(A, B, C).

-spec build(p_query()) -> b_query().
build(A) -> mekao_core:build(A).