/*
 * Prolog library for running an embedded Julia instance
 * Samer Abdallah (2017)
*/

:- module(julia, [
      jl_open/0
   ,  jl_close/0
   ,  jl_exec/1
   ,  jl_eval/2
   ,  (?)/1, (<?)/2, (?>)/2
   ,  op(900,fx,?)
   ,  op(900,xfy,<?)
   ,  op(900,yfx,?>)
   ]).

/** <module> Embedded Julia
*/
:- reexport(dcg/julia).

:- use_foreign_library(foreign(julia4pl)).

?(Expr) :-
   term_jlstring(ans=Expr,Str),
   debug(julia, 'Executing: ~s',[Str]),
   jl_exec(Str),
   jl_exec("display(ans)"), nl.

<?(Result, Expr) :- Expr ?> Result.
?>(Expr, Result) :-
   term_jlstring(Expr,Str),
   debug(julia, 'Evaluating: ~s',[Str]),
   jl_eval(Str,Result).


