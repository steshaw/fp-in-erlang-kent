# Functional Programming in Erlang

This is my working through the course [Functional Programming in Erlang](https://www.futurelearn.com/courses/functional-programming-erlang/) offered by the [University of Kent](https://www.kent.ac.uk/) on the [FutureLearn](http://futurelearn.com/) platform.


## Some Notes

### Erlang data types

"six" Erlang data types were introduced:

- numbers (integers/bignums and floats)
- atoms
- booleans
- tuples and lists
- strings
- functions

It seemed that integers and floats are separate data types as are tuples and lists. Whereas booleans seemed to be simply 2 special atoms (aka symbols): 'true' and 'false'. Strings appeared to be lists of numbers (or at least numbers in the character-set range). My modified list might be:

- integers
- floats
- atoms
- tuples
- lists
- functions

Still six ;-).

The [reference manual lists some other data types](http://erlang.org/doc/reference_manual/data_types.html) which must be covered later on.

- Bit String and Binaries
- Reference
- Port Identifier
- Pid
- Map
- Record


### Pattern matching

Erlang pattern matching is somewhat different to Haskell's. In Haskell, the variables are always "new" (aka "fresh") but in Erlang they can be bound already. If they are bound then they must must the corresponding structure or the whole match fails.

Something without a direct equivalent in Haskell:

```erlang
erl> {A, A} = {2, 2}.
{2,2}
erl> A.
2
```

Notice that the match would fail against ```erlang{2, 3}```.
