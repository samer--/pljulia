:- use_module(library(julia)).

:- set_prolog_flag(back_quotes, symbol_char).

:- initialization(init_env, program).
:- initialization(init_jl, program).

:- op(950, fx, jpl).

init_env :-
   persistent_history,
   debug(pljl).

init_jl  :-
   ??using('Plots'),
   backend(plotlyjs).
   maplist(default, [framestyle, colorbar, show], [:box, false, true]).

default(K,V) :- ??default(:K, V).
colormap(M) :- ??default(:color, :M).
backend(B)  :- member(B,[plotlyjs, unicodeplots, gr, glvisualization]), ?? B@[].

/*
 clibraries()
 showlibrary(Lib)
*/


