# pljulia
Embedded Julia for SWI Prolog


This module allows Prolog code to use an embedded instance of Julia,
a language for numerical and technical computing.


## INSTALLATION

Make sure you have installed Julia, and that the julia executable is on your PATH.

Then do

    ?- pack_install(pljulia).


## USAGE

    ?- use_module(library(julia)).
    ?- ?cumsum(rand(4,4)).
    ?- X <? 6*7.
    ?- "hello world"[1:2:end] ?> X.

See examples/example.pl for more, including plotting.

## TODO

Handle more Julia types directly, tuples and arrays.

Handle more node types in Julia syntax trees.

Consider having `jl_array_t` structures floating around as Prolog BLOB
atoms, assuming they can be protected from Julia's garbage collector.
Should be more efficient than converting to/from Prolog lists when dealing
with numeric arrays.

Handle more Julia syntax:

    ; to separate positional args from keyword args
    {} for parametric types

Consider allowing Julia to call back to Prolog.
