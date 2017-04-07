-module(ss_lists).
-export(
 [ product/1
 , product_bench/1
 , product_tr/1
 , product_tr_bench/1
 , maximum/1
 , maximum_tr/1
 , double/1
 , evens/1
 , median/1
 , median_test/0
 , average/2
 , nth/2
 , nth_test/0
 , sort/1
 , sort_test/0
 , insert/2
 , modes/1
 , modes_test/0
 , occurences/2
 , unique/1
 , uniq/1
 ]
).

%%
%% @doc The product of a list of numbers.
%%
-spec product([number()]) -> number().
product([]) ->
  % The product of an empty list is usually taken to be 1: why?
  % The number 1 is the multiplicative identity.
  1;
product([X | XS]) -> X * product(XS).

%%
%% @doc a long-running execution of `bench'.
%% @returns number of seconds to execute
%%
product_bench(N) ->
  {Time, _} = timer:tc(fun() ->
    product(lists:seq(1, N)),
    nothing
  end),
  Time / 1000000.

-spec product_tr([number()]) -> number().
product_tr(XS) -> product_tr(XS, 1).

-spec product_tr([number()],number()) -> number().
product_tr([], Product) -> Product;
product_tr([X | XS], Product) -> product_tr(XS, X * Product).

%%
%% @doc a long-running execution of `bench'.
%% @returns number of seconds to execute
%%
product_tr_bench(N) ->
  {Time, _} = timer:tc(fun() ->
    product_tr(lists:seq(1, N)),
    nothing
  end),
  Time / 1000000.

%%
%% @doc non tail-recursive maximum of a list of numbers.
%%
-spec maximum([number()]) -> number().
maximum([X]) -> X;
maximum([X | XS]) -> max(X, maximum(XS)).

-spec maximum_tr([number()]) -> number().
maximum_tr([X | XS]) -> maximum_tr(XS, X).
maximum_tr([], Maximum) -> Maximum;
maximum_tr([X | XS], Maximum) -> maximum_tr(XS, max(X, Maximum)).

% Which of the two styles – direct recursion and tail recursion – do you
% find most natural? Why?
% The _direct_ recursion style is most natural. It follows the mathematical
% process of "base case" and "induction case".

-spec double([number()]) -> [number()].
double([]) -> [];
double([N | NS]) -> [2 * N | double(NS)].

-spec is_even(integer()) -> boolean().
is_even(N) -> N rem 2 == 0.

-spec evens([integer()]) -> [integer()].
evens([]) -> [];
evens([N | NS]) ->
  case is_even(N) of
    true  -> [N | evens(NS)];
    false -> evens(NS)
  end.

%
% @doc
% The median of a list of numbers.
% This is the middle element when the list is ordered (if the list is of
% even length you should average the middle two).
%
-spec median(nonempty_list(number())) -> number().
median([_ | _] = Ns) -> % only works for non-empty lists.
  Sorted = sort(Ns),
  Len = length(Ns),
  case is_even(Len) of
    true ->
      % Length is even, average the middle two elements.
      % .e.g median([1,2,3,4]) => average(2, 3).
      First = Len div 2,
      average(nth(First, Sorted), nth(First + 1, Sorted));
    false ->
      % Length is odd, pick the middle element.
      % e.g. median([1, 2, 3]) => 2.
      Middle = (Len div 2) + 1,
      nth(Middle, Sorted)
  end.

%
% @doc Unit test for `median'.
% @see median/1
% @returns Expected: [true, true].
%
median_test() ->
  [ median([4, 3, 2, 1]) == average(2, 3)
  , median([3, 2, 1]) == 2
  ].

average(A, B) -> (A + B) / 2.

% The modes of a list of numbers: this is a list consisting of the numbers
% that occur most frequently in the list; if there is is just one, this will
% be a list with one element only.

% Other uses functions.

%
% @doc
% Sort the list.
%
sort([]) -> [];
sort([X]) -> [X];
sort([X | Xs]) -> insert(X, sort(Xs)).

%
% @doc Unit test for `sort'.
% @see sort/1
% @returns Expect list of trues.
%
sort_test() ->
  [ sort([]) == []
  , sort([1]) == [1]
  , sort([1,2,3]) == [1,2,3]
  , sort([3,2,1]) == [1,2,3]
  , sort([2,3,1]) == [1,2,3]
  , sort([2,1,3]) == [1,2,3]
  ].

%
% @doc
% Insert X into a sorted list.
%
insert(X, []) -> [X];
insert(X, [Y | Ys] = Xs) ->
  case X < Y of
    true  -> [X | Xs];
    false -> [Y | insert(X, Ys)]
  end.

%
% @doc
% The nth element of the list.
%
nth(1, [X | _ ]) -> X;
nth(N, [_ | Xs]) -> nth(N - 1, Xs).

%
% @doc Unit test for `nth'.
% @see nth/2
% @returns Expecting [true, true, true].
%
nth_test() ->
  [ nth(1, [1, 2, 3]) == 1
  , nth(2, [1, 2, 3]) == 2
  , nth(3, [1, 2, 3]) == 3
%  , nth(4, [1, 2, 3]) == 1/0
  ].

%
% @doc
% The modes of a list of numbers.
% This is a list consisting of the numbers that occur most frequently
% in the list; if there is is just one, this will be a list with one
% element only
%
% @end
% Sigh, this is a very slow implementation because of the number
% of traversals through the list.
%
-spec modes([number()]) -> [number()].
modes([]) -> [];
modes(Ns) ->
  FrequencyMaps = frequencies(Ns),
  Frequencies = map(fun ({_, F}) -> F end, FrequencyMaps),
  MaximumFrequency = maximum(Frequencies),
  MaximumFrequencyMaps = filter(fun ({_, F}) -> F == MaximumFrequency end, FrequencyMaps),
  map(fun ({N, _}) -> N end, MaximumFrequencyMaps).

%
% @doc Unit test for `modes'.
% @see modes/1
% @returns a list of trues.
%
modes_test() ->
  [ modes([]) == []
  , modes([1]) == [1]
  , modes([1,2,3,4]) == [1,2,3,4]
  , modes([1,2,2,3,4]) == [2]
  , modes([6,4,5,9,2,5,6]) == [5, 6]
  ].

%
% @doc
% The frequencies/occurences of each unique element of a list.
% Returns a list of tuples {N, occurrences(N)}.
%
frequencies(Ns) ->
  Us = unique(Ns),
  map(fun(N) -> {N, occurences(N, Ns)} end, Us).

%
% @doc
% Return only the unique elements of a list.
%
unique(Xs) -> uniq(sort(Xs)).

%
% @doc
% Removes adjacent duplicate elements.
%
uniq([]) -> [];
uniq([X, X | Xs]) -> uniq([X | Xs]);
uniq([X | Xs]) -> [X | uniq(Xs)].

%
% @doc
% Count the occurrences of a value in a list.
%
occurences(_, []      ) -> 0;
occurences(X, [X | Xs]) -> 1 + occurences(X, Xs);
occurences(X, [_ | Xs]) -> occurences(X, Xs).

%
% @doc
% non-tail-recursive map/transform.
%
map(_, []) -> [];
map(F, [X | Xs]) -> [F(X) | map(F, Xs)].

%
% @doc
% non-tail-recursive filter.
%
filter(_, []) -> [];
filter(F, [X | Xs]) ->
  case F(X) of
    true  -> [X | filter(F, Xs)];
    false -> filter(F, Xs)
  end.
