-module(assignment).
-export(
  [ area/1
  , area_test/0
  , perimeter/1
  , perimeter_test/0
  , enclose/1
  , enclose_test/0
  ]).

%
% Shapes are represented using the following:
%
%   {circle, R}
%   {rectangle, H, W}
%   {triangle, A, B, C}
%

area({circle, R}) ->
  math:pi() * R * R;
area({rectangle, H, W}) ->
  H * W;
area({triangle, A, B, C}) ->
  S = (A + B + C) / 2,
  math:sqrt(S * (S - A) * (S - B) * (S - C)).

perimeter({circle, R}) ->
  2 * math:pi() * R;
perimeter({rectangle, H, W}) ->
  2 * (H + W);
perimeter({triangle, A, B, C}) ->
  A + B + C.

% return the smallest enclosing rectangle.
enclose({circle, R}) ->
  Diameter = 2 * R,
  {rectangle, Diameter, Diameter};
enclose({rectangle, H, W}) ->
  {rectangle, H, W};
enclose({triangle, A, B, C}) ->
  % Find the maximum side to use as the base.
  % This avoids problems with obtuse triangles (if we
  % simply picked a side to be the base).
  Base = max(max(A, B), C),
  % The perpendicular height given the base.
  Height = 2 * area({triangle, A, B, C}) / Base,
  {rectangle, Base, Height}.

% Example shapes for tests.
circle1() -> {circle, 3.0}.
rectangle1() -> {rectangle, 3.0, 5.0}.
triangle1() -> {triangle, 3.0, 4.0, 5.0}.

% Result should be [true,true,true].
area_test() ->
  A1 = area(circle1()),
  R1 = (A1 > 28.27) and (A1 < 28.28),
  R2 = area(rectangle1()) == 15.0,
  R3 = area(triangle1()) == 6.0,
  [R1, R2, R3].

% Result should be [true,true,true].
perimeter_test() ->
  P1 = perimeter(circle1()),
  R1 = (P1 > 18.849) and (P1 < 18.850),
  R2 = perimeter(rectangle1()) == 16.0,
  R3 = perimeter(triangle1()) == 12.0,
  [R1, R2, R3].

% Result should be [true,true,true].
enclose_test() ->
  R1 = enclose(circle1()) == {rectangle, 6.0, 6.0},
  R2 = enclose(rectangle1()) == rectangle1(),
  R3 = enclose(triangle1()) == {rectangle, 5.0, 2.4},
  [R1, R2, R3].
