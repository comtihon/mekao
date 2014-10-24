#Specs

This document tries to describes my vision on how to use specs with *mekao*.

Several times I heard similar question regarded `ets:select/2`: I have a record
but *dialyzer* complains when I put atom `'_'` as a field's value, what should I
do?  The same problem applicable to *mekao* usage — some of the values have
special meaning (i.e. `'$skip'` or `{'$predicate, ...'}`).

Typical record may look like this:

```erlang
-record(book, {
    id      :: positive_integer(),
    isbn    :: binary(),
    title   :: binary(),
    author  :: binary(),
    created :: calendar:datetime()
}).
```
When we want to select a records by it's author we could write something like
this:

```erlang
mekao:select(#book{author = <<"Joe">>, _ = '$skip'}, Table, Settings).
```
That is direct violation of the spec, because non of the fields of the `#book{}`
were allowed to put atom `'$skip'` as a value.

One of the possible solutions may be just to add this and other special terms
to the spec like this:

```erlang
-record(book, {
    id      :: '$skip' | positive_integer(),
    isbn    :: '$skip' | binary(),
    title   :: '$skip' | binary(),
    author  :: '$skip' | binary(),
    created :: '$skip' | calendar:datetime()
}).
```
But I suggest not to do this and here is why. I found that regular code uses
special values only when in need to communicate with DB, in other cases it tend
to work with ordinary record values. And each such communication returns
ordinary record too. So I use to divide specs into 3 groups like this:

```erlang
%% record without the specs
-record(book, {id, isbn, title, author, created}).

%% parametrized typespec for the #book{}
-type book(E) :: #book{
    id      :: E | positive_integer(),
    isbn    :: E | binary(),
    title   :: E | binary(),
    author  :: E | binary(),
    created :: E | calendar:datetime()
}.

%% typespec for regular usage
-type book() :: book(none()).

%% typespec for inserts and updates(set clause)
-type book_inserter() :: book('$skip').

%% typespec for selects, deletes and updates(where clause)
-type book_selector() :: #book{
    id      :: mekao:predicate(positive_integer()),
    isbn    :: mekao:predicate(binary()),
    title   :: mekao:predicate(binary()),
    author  :: mekao:predicate(binary()),
    created :: mekao:predicate(calendar:datetime())
}.
```

From now on you can safely create record instance with any value, but when you
will try to pass it around, function with corresponding spec will warn you about
violation.

This is seems like overkill, that is why I am not insist on this, feel free to
raise an issue and to suggest different solution.
