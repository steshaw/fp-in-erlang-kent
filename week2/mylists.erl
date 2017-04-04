-module(mylists).
-export(
 [ product/1
 , product_tr/1
 , maximum/1
 , maximum_tr/1
 , double/1
 , evens/1
 ]
).

product([]) ->
  % The product of an empty list is usually taken to be 1: why?
  % The number 1 is the multiplicative identity.
  1;
product([X | XS]) -> X * product(XS).

product_tr(XS) -> product_tr(XS, 1).
product_tr([], Product) -> Product;
product_tr([X | XS], Product) -> product_tr(XS, X * Product).

maximum([X]) -> X;
maximum([X | XS]) -> max(X, maximum(XS)).

maximum_tr([X | XS]) -> maximum_tr(XS, X).
maximum_tr([], Maximum) -> Maximum;
maximum_tr([X | XS], Maximum) -> maximum_tr(XS, max(X, Maximum)).

% Which of the two styles – direct recursion and tail recursion – do you
% find most natural? Why?
% The _direct_ recursion style is most natural. It follows the mathematical
% process of "base case" and "induction case".

double([]) -> [];
double([N | NS]) -> [2 * N | double(NS)].

is_even(N) -> N rem 2 == 0.

evens([]) -> [];
evens([N | NS]) ->
  case is_even(N) of
    true  -> [N | evens(NS)];
    false -> evens(NS)
  end.
