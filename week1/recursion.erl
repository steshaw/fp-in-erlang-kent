-module(recursion).
-export(
  [ factorial/1
  , fib/1
  , pieces1/1
  , pieces2/1
  ]).

factorial (0) -> 1;
factorial (N) when N > 0 -> factorial(N-1) * N.

% Fibonacci sequence: 0, 1, 1, 2, 3, 5, ….
fib (0) -> 0;
fib (1) -> 1;
fib (N) when N > 1 -> fib(N-2) + fib(N-1).

% formula found at http://math.stackexchange.com/a/646433/96366
pieces1 (N) when N >= 0 -> (N*N + N + 2) div 2.

% recursive formula found at http://math.stackexchange.com/a/169436/96366
pieces2 (0) -> 1;
pieces2 (N) when N > 0 -> pieces2(N - 1) + N.
