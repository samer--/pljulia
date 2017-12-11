:- use_module(library(pljulia)).

:- set_prolog_flag(back_quotes, symbol_char).

:- initialization(init_env, program).
:- initialization(init_jl, program).

:- op(950, fx, jpl).

init_env :-
   persistent_history,
   debug(pljl).

init_jl  :- ??using('Plots'), backend(plotlyjs).
default(K,V) :- ??default(:K, V).
colormap(M) :- ??default(:color, :M).
backend(B)  :- member(B,[plotlyjs, unicodeplots, gr, glvisualization]), ?? B@[].

:- maplist(default, [framestyle, colorbar, show], [:box, false, true]).
/*
 clibraries()
 showlibrary(Lib)
*/


