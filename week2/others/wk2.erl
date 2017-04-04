%% From https://hastebin.com/ukevotoyel.erl

%% Week 2 Stuff

-module(wk2).

-export(
  [ head/1
  , tail/1
  , second/1
  , sum/1
  , sumTail/1
  , product/1
  , productTail/1
  , maximum/1
  , maxTail/1
  ]
).

%% head and tail
head([]) -> [];
head([X|_Xs]) -> X.
tail([]) -> [];
tail([_X|Xs]) -> Xs.

%% second.
second([X|Xs]) -> head(tail([X|Xs])).

%% sum: add up all the elements in a list using direct recursion.
sum([]) -> 0;
sum([X|Xs]) -> X + sum(Xs).

%%sumTail: add up all the elements in a list using tail recursion.
sumTail([]) -> 0;  %% return 0 for an empty list like sum does.
sumTail([X|Xs]) -> sumTail([X|Xs],0).  %% convert to 2 arguments by adding accumulator.
sumTail([],S) -> S;  %% return the sum when list exhausted.
sumTail([X|Xs],S) -> sumTail(Xs,S+X).  %% add in X and repeat.


%% product: return the product of a list of numbers.
product([]) -> 1;
product([X|Xs]) -> X * product(Xs).
%%productTail: return the product of a list of numbers using tail recursion.
productTail(Xs) -> productTail(Xs,1).
productTail([],P) -> P;
productTail([X|Xs],P) -> productTail(Xs,P*X).


%% maximum: return the highest value in a list.
maximum([X]) -> X;
maximum([X|Xs]) -> maximum([max(X,head(Xs))|tail(Xs)]).  %% max of 1st 2 elements then rest of list recursively.

%% maxTail: return highest value in a list via tail recursion.
maxTail([]) -> [];
maxTail([X|Xs]) -> maxTail(X,Xs).  %% Use 1st argument for storing max so far.
maxTail(M,[]) -> M;  %% return the highest value when list exhausted.
maxTail(M,[X|Xs]) -> maxTail(max(M,X),Xs).  %% Check head against M then repeat.
