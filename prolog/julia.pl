/*
 * Prolog library for running an embedded Julia instance
 * Samer Abdallah (2017)
*/

:- module(julia, [
      jl_exec/1
   ,  jl_eval/2
   ,  jl_call/3,  jl_call_/2
   ,  jl_call/4,  jl_call_/3
   ,  jl_call/5,  jl_call_/4
   ,  jl_apply/3, jl_apply_/2
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
   val == integer | float | string | bool | symbol | void | array_int64(_) | array_float64(_)

   bool   ---> true; false.
   symbol ---> :atom.
   void   ---> nothing.
   array_int64(D)   ---> int64(shape(D), nested(D,integer)).
   array_float64(D) ---> float64(shape(D), nested(D,float)).
   ==
   These types are mapped to and from Julia types as follows:
   ==
   Int64   <--> integer
   Float64 <--> float
   String  <--> string
   Symbol  <--> symbol
   Bool    <--> bool
   Void    <--> void
   ==
   A D-dimensional arrays is represented using a shape(D), which is list of
   exactly D integers, in REVERSE ORDER from the Julia form, and a D-level nested
   list containing the values, where the first dimension is the length of the outer
   list, the second dimension is the length of each 1st level nested list, and so on.

   These arrays can be passed to jl_call/N, returned from jl_eval, and also
   passed to the Julia DCG via the higher-level term level evaluators/executors.
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
%  Any other return type raises an =|unsupported_julia_return_type_error|= exception.
%  If any Julia exceptions are raised, the Julia function =|showerror|= is used
%  to print the details, and a Prolog =|julia_error|= exception is raised.

%! jl_call(+F:text, +Arg:val, -Result:val) is det.
%! jl_call(+F:text, +Arg1:val, +Arg2:val, -Result:val) is det.
%! jl_call(+F:text, +Arg1:val, +Arg2:val, +Arg3:val, -Result:val) is det.
%  Evaluates F to get a function and applies it to given arguments.
%  The arguments are converted to Julia values directly in C code, which
%  should be faster than conversion via DCG formatting and parsing in Julia.
jl_call(F,X,R) :- jl_apply(F,1,[X],R).
jl_call(F,X,Y,R) :- jl_apply(F,2,[X,Y],R).
jl_call(F,X,Y,Z,R) :- jl_apply(F,3,[X,Y,Z],R).

%! jl_call_(+F:text, +Arg:val) is det.
%! jl_call_(+F:text, +Arg1:val, +Arg2:val) is det.
%! jl_call_(+F:text, +Arg1:val, +Arg2:val, +Arg3:val) is det.
%  Same as jl_call/3, jl_call/4 etc but ignoring the return value.
%  This can be useful when the return value is not supported by pljulia.
jl_call_(F,X) :- jl_apply_(F,1,[X]).
jl_call_(F,X,Y) :- jl_apply_(F,2,[X,Y]).
jl_call_(F,X,Y,Z) :- jl_apply_(F,3,[X,Y,Z]).

%! jl_apply(+F:text, +Args:list(val), -Result:val) is det.
%  Evaluates Julia expression F to a function and applies it to any number
%  of arguments in Args, unifying the return value with Result.
jl_apply(F,Args,R) :- length(Args,N), jl_apply(F,N,Args,R).

%! jl_apply_(+F:text, +Args:list(val)) is det.
%  Evaluates Julia expression F to a function and applies it to any number
%  of arguments in Args and discarding the return value.
jl_apply_(F,Args) :- length(Args,N), jl_apply_(F,N,Args).



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
prolog:message(error(unsupported_julia_return_type(C,T),_)) -->
   ['Cannot convert Julia type `~s\' to Prolog value in context ~s'-[T,C]].
