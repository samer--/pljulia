/*
 * Prolog library for running an embedded Julia instance
 * Samer Abdallah (2017)
*/

:- module(julia, [
      jl_exec/1
   ,  jl_eval/2
   ,  jl_call/3
   ,  jl_call/4
   ,  (!)/1, (?)/1, (<?)/2, (?>)/2
   ,  op(900,fx,?)
   ,  op(900,fx,!)
   ,  op(900,xfy,<?)
   ,  op(900,yfx,?>)
   ]).

/** <module> Embedded Julia

   When loaded, this module starts an embedded instance of Julia.
   Expressions may be executed (either discarding the return value or
   printing it in julia) or evaluated, unifying certain Julia return types
   with Prolog terms.

   See library(dcg/julia) for description of valid expression terms.

	The type =|val|= denotes values that can be exchanged with Julia by
	direct conversion, rather than via strings. It is a union:
	==
	val == integer | float | string | bool | symbol | void

	bool   ---> true; false.
	symbol ---> :atom.
	void   ---> nothing.
	==
*/

:- use_foreign_library(foreign(julia4pl)).
:- reexport(dcg/julia).

%! jl_exec(+S:text) is det.
%  Execute Julia expression S, which may be string, atom, codes or chars.
%  If any Julia exceptions are raised, the Julia function =|showerror|= is used
%  to print the details, and a =|julia_error|= exception is raised.

%! jl_eval(+S:text, -X:val) is det.
%  Evaluate Julia expression S, which may be string, atom, codes or chars.
%  The following Julia return types are recognised and are converted to Prolog values
%  and unified with X:
%  ==
%  Int64   -> integer
%  Float64 -> float
%  String  -> string
%  Symbol  -> symbol (where symbol ---> :atom)
%  Bool    -> bool (where bool ---> true; false)
%  ==
%  In addition, the Julia value =|nothing|= is represented as the Prolog atom =|nothing|=.
%  Any other return type raises an =|unsupported_julia_return_type_error|= exception.
%  If any Julia exceptions are raised, the Julia function =|showerror|= is used
%  to print the details, and a Prolog =|julia_error|= exception is raised.

%! jl_call(+F:text, +Arg:val, -Result:val) is det.
%! jl_call(+F:text, +Arg1:val, +Arg2:val, -Result:val) is det.
%  Evaluates F to get a function and applies it to given arguments.
%  The arguments are converted to Julia values directly in C code, which
%  should be faster than conversion via DCG formatting and parsing in Julia.

%! !(+E:expr) is det.
%  Execute Julia expression E using jl_exec/1. No output is printed.
%  E is converted to a string using dcg_julia:term_jlstring/2.
%  Use =|debug(julia)|= to see the Julia expression before execution.
!(Expr) :-
   term_jlstring(Expr,Str),
   debug(julia, 'Executing: ~s',[Str]),
   jl_exec(Str).

%! ?(+E:expr) is det.
%  Execute Julia expression term E using (!)/1 and display the result using Julia.
?(Expr)          :- !(ans=Expr :>: display(ans)), nl.

%! ?>(+E:expr, -X:val) is det.
%  Evaluate the Julia expression term E using jl_eval/2 and unify result with X.
%  E is converted to a string using dcg_julia:term_jlstring/2.
%  Use =|debug(julia)|= to see the Julia expression before execution.
?>(Expr, Result) :-
   term_jlstring(Expr,Str),
   debug(julia, 'Evaluating: ~s',[Str]),
   jl_eval(Str,Result).

%! <?(-X:val, +E:expr) is det.
%  Evalute Julia expression E and unify result with X. =|X <? E|= is the
%  same as =|E ?> X|=, but may be more convenient for interactive use.
<?(Result, Expr) :- Expr ?> Result.

prolog:message(error(julia_error,_)) --> ['A Julia exception was thrown.'].
prolog:message(error(unsupported_julia_return_type_error(T),_)) -->
   ['Cannot convert Julia type `~s\' to Prolog value'-[T]].
