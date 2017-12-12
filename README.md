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
