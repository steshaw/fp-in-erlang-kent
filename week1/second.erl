-module(second).
-export([
  hypotenuse/2,
  perimeter/2,
  area/2
]).

hypotenuse(L, H) ->
  math:sqrt(first:square(L) + first:square(H)).

perimeter(L, H) ->
  L + H + hypotenuse(L, H).

area(L, H) ->
  L * H / 2.
