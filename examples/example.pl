/*
   pljulia examples. Requires Plots.jl package
   and some graphics backends, such as PlotlyJS.pl
   These are installed in Julia with

      Pkg.add("Plots")
      Pkg.add("PlotlyJS")
*/
:- use_module(library(julia)).

:- set_prolog_flag(back_quotes, symbol_char).

:- initialization(init_env, program).
:- initialization(init_jl, program).

init_env :- debug(julia).
init_jl  :-
   ?using('Plots'),
   backend(plotlyjs),
   maplist(default, [framestyle- :box, colorbar-false, show-true]).

default(K-V) :- ?default(:K, V).
colormap(M) :- ?default(:color, :M).
backend(B)  :- member(B,[plotlyjs, unicodeplots, gr, glvisualization]), ? B@[].

eg(brownian_motion) :- ?plot(cumsum(randn(500,3)), xlabel="time", ylabel="cromulence").
eg(heatmap) :- ?heatmap(cumsum(randn(50,50)`), color= :fire).
eg(gradients(Lib)) :-
   N <? length(clibraries()),
   between(1,N,I),
   Lib <? clibraries()[I],
   ?showlibrary(Lib).
