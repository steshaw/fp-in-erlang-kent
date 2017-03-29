-module(assignment).
-export(
  [ perimeter/1
  ]).

% {circle, {X, Y}, R}
% {rectangle, {X, Y}, H, W}

perimeter({circle, {_X, _Y}, R}) ->
  2 * math:pi() * R;
perimeter({rectangle, {_X, _Y}, H, W}) ->
  2 * (H + W).
