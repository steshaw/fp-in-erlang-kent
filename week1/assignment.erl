-module(assignment).
-export(
  [ perimeter/1
  , perimeter_test/0
  ]).

% {circle, {X, Y}, R}
% {rectangle, {X, Y}, H, W}
% {triangle, {X, Y}, A, B, C}

perimeter({circle, {_X, _Y}, R}) ->
  2 * math:pi() * R;
perimeter({rectangle, {_X, _Y}, H, W}) ->
  2 * (H + W);
perimeter({triangle, {_X, _Y}, A, B, C}) ->
  A + B + C.

perimeter_test() ->
  P1 = perimeter({circle, {0, 0}, 3}),
  R1 = (P1 > 18.849) and (P1 < 18.850),
  R2 = perimeter({rectangle, {0, 0}, 3.0, 5.0}) == 16.0,
  R3 = perimeter({triangle, {0, 0}, 3.0, 4.0, 5.0}) == 12.0,
  [R1, R2, R3].
