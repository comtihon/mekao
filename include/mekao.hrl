-record(mekao_query, {
    body,
    types  = []     :: [term()],
    values = []     :: [term()],
    next_ph_num = 1 :: non_neg_integer()
}).

-record(mekao_select, {
    columns     :: mekao:iotriple(),
    table       :: mekao:iotriple(),
    where       :: mekao:iotriple(),
    order_by    :: mekao:iotriple()
}).

-record(mekao_insert, {
    table       :: mekao:iotriple(),
    columns     :: mekao:iotriple(),
    values      :: mekao:iotriple(),
    returning   :: mekao:iotriple()
}).

-record(mekao_update, {
    table       :: mekao:iotriple(),
    set         :: mekao:iotriple(),
    where       :: mekao:iotriple(),
    returning   :: mekao:iotriple()
}).

-record(mekao_delete, {
    table       :: mekao:iotriple(),
    where       :: mekao:iotriple(),
    returning   :: mekao:iotriple()
}).

-record(mekao_column, {
    name        :: iodata(),        %% sql column name
    type        :: term(),          %% sql datatype, acceptable by underlying
                                    %% driver
    key = false :: boolean(),       %% primary key part
    ro  = false :: boolean(),       %% readonly
    transform   :: undefined
                 | fun ((Val :: term()) -> NewVal :: term())
}).

-record(mekao_table, {
    name            :: iodata(),
    columns = []    :: [mekao:column()],
    %% order by column position or by arbitrary expression
    order_by = []   :: [ non_neg_integer() % record's field pos
                       | iodata()          % arbitrary expression
                       | { non_neg_integer() | iodata()
                         , { asc | desc | default
                           , nulls_first | nulls_last | default}
                         }
                       ]
}).

-record(mekao_settings, {
    placeholder :: fun( ( mekao:column()
                        , Num :: non_neg_integer()
                        , Val :: term()
                        ) -> iodata()),

    limit :: undefined
           | fun( ( mekao:'query'(#mekao_select{})
                  , RowCount :: non_neg_integer()
                  , Offset :: non_neg_integer()
                  ) -> mekao:'query'(#mekao_select{})),

    returning :: undefined
               | fun(( insert | update | delete, mekao:table()) -> iodata()),

    is_null = fun mekao_utils:is_null/1 :: fun((Value :: term()) -> boolean())
}).

-type iotriple() :: iodata() | {iodata(), iodata(), iodata()}.

-type table() :: #mekao_table{}.
-type column() :: #mekao_column{}.
-type s() :: #mekao_settings{}.

-type entity() :: tuple() | list(term() | '$skip').
-type selector() :: tuple() | list(predicate()).

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