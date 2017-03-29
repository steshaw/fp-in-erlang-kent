-module(tail).
-export(
  [ loop/1
  , sum0/2
  , sum1/2
  , max0/2
  , max1/2
  , fib0/1
  , fib1/1
  , perfect0/1
  , perfect1/1
  ]).

loop(N) when N > 0 ->
  io:format("~p~n", [N]),
  loop(N - 1);
loop(_) ->
  io:format("bye~n").

% non tail-recursive.
sum0(F, 0) -> F(0);
sum0(F, N) when N > 0 -> F(N) + sum0(F, N - 1).

% tail-recursive.
sum1(F, N) -> sum1(F, N, F(0)).
sum1(_, 0, Acc) -> Acc;
sum1(F, N, Acc) when N > 0 -> sum1(F, N-1, Acc + F(N)).

% non tail-recursive.
max0(F, 0) -> F(0);
max0(F, N) -> max(F(N), max0(F, N - 1)).

% tail-recursive.
max1(F, N) -> max1(F, N, F(0)).
max1(_, 0, Acc) -> Acc;
max1(F, N, Acc) when N > 0 -> max1(F, N-1, max(Acc, F(N))).

% non tail-recursive.
fib0(0) -> 0;
fib0(1) -> 1;
fib0(N) -> fib0(N-2) + fib0(N-1).

fib1(N) when N >= 0 -> fib1(N, 0, 1).
fib1(0, Nm2, _) -> Nm2;
fib1(N, Nm2, Nm1) -> fib1(N-1, Nm1, Nm2 + Nm1).

%
% Evaluation of fib1(4):
%
% -> fib1(4)
% -> fib1(4, 0, 1)
% -> fib1(3, 1, 1)
% -> fib1(2, 1, 2)
% -> fib1(1, 2, 3)
% -> fib1(0, 3, 5)
%

% Correct but inefficient program. Laziness would help here... :).
perfect0(N) when N > 0->
  L = lists:seq(1, N div 2),
  Divisors = lists:filter(fun(Divisor) -> N rem Divisor == 0 end, L),
  N == lists:sum(Divisors).

% More efficient but less perspicuous.
perfect1(N) when N > 0 -> perfect1(N, 1, 0).
perfect1(N, Divisor, Acc) when Divisor > (N div 2) ->
  N == Acc;
perfect1(N, Divisor, Acc) ->
  Acc1 = if
    N rem Divisor == 0 -> Acc + Divisor;
    true -> Acc
  end,
  perfect1(N, Divisor+1, Acc1).
