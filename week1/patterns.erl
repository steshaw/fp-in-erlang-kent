-module(patterns).
-export(
  [ xor0/2
  , xor1/2
  , xor2/2
  , xor3/2
  , xor4/2
  , xor5/2
  , test/1
  , testAll/0
  , maxThree1/3
  , maxThree2/3
  , howManyEqual1/3
  , howManyEqual2/3
  ]).

% built-in xor.
xor0 (A, B) -> A xor B.

xor1 (true, false) -> true;
xor1 (false, true) -> true;
xor1 (_, _)        -> false.

xor2 (A, A) -> false;
xor2 (_, _) -> true.

xor3 (false, false) -> false;
xor3 (false, true) -> true;
xor3 (true, false) -> true;
xor3 (true, true) -> false.

xor4 (A, B) -> A =/= B.

xor5 (A, B) -> (not A) == B.

xor6 (A, B) -> not (A == B).

% Not sure what precedence we get here.
xor7 (A, B) -> not A == B.

xor8 (A, B) -> (not A and B) or (A and not B).

toChar (true) -> $.;
toChar (false) -> $X.

test (Xor) ->
  [ toChar(Xor(false, false) == false)
  , toChar(Xor(false, true) == true)
  , toChar(Xor(true, false) == true)
  , toChar(Xor(true, true) == false)
  ].

testAll () ->
  F = fun({Name, F}) ->
    {Name, test(F)}
  end,
  Xors =
    [ {xor0, fun xor0/2}
    , {xor1, fun xor1/2}
    , {xor2, fun xor2/2}
    , {xor3, fun xor3/2}
    , {xor4, fun xor4/2}
    , {xor5, fun xor5/2}
    , {xor6, fun xor6/2}
    , {xor7, fun xor7/2}
    , {xor8, fun xor8/2}
    ],
  lists:map(F, Xors).

maxThree1(A, B, C) -> max(max(A, B), C).
maxThree2(A, B, C) -> lists:max([A, B, C]).

howManyEqual1(A, A, A) -> 3;
howManyEqual1(A, A, _) -> 2;
howManyEqual1(_, A, A) -> 2;
howManyEqual1(A, _, A) -> 2;
howManyEqual1(_, _, _) -> 0.

howManyEqual2(A, B, C) ->
  L = [A, B, C],
  F = fun(A) ->
    lists:map(fun(B) ->
      case A == B of
        true  -> 1;
        false -> 0
      end
    end, L)
  end,
  R = lists:max(lists:map(fun lists:sum/1, lists:map(F, L))),
  if R == 1 -> 0; true -> R end.
