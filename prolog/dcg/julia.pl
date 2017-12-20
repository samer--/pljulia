/*
 * Part of pljulia
 * Copyright Samer Abdallah 2017
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

:- module(dcg_julia,
      [  term_jlstring/2   % (+Expr, -String)  ~Prolog term to Julia string
      ,  op(150,fx,:)      % symbols
      ,  op(160,yf,'`')    % postfix ctranspose operator
      ,  op(800,xfy,:>:)
      ,  op(210,xfy,.^)    % broadcasting exponentiation
      ,  op(410,yfx,.*)    % broadcasting times
      ,  op(410,yfx,./)    % broadcasting division
      ,  op(410,xfy,.\)    % reverse broadcasting division
      ,  op(400,xfy,\)     % reverse matrix division
      ,  op(100,yfx,'`')   % dot, as in modules and fields
      ,  op(750,xfy,\\)    % lambda abdstraction
      ,  op(200,yf,[])     % array indexing
      ,  op(160,fx,.)      % broadcasting op
      ,  op(200,yfx,@)     % function application
      ,  op(200,yfx,.@)    % broadcasting function application
      ,  op(300,yf,...)    % splicing
      ,  op(700,xfy,=>)    % dictionary key-value pair
      ]).


:- multifile user:pl2jl_hook/2, pl2jl_hook/3.

/** <module> Julia DCG

   TODO

   @ for macros?
   ; for keyword arguments

   REVIEW
    \\ for lambda

   ---++++ Julia expression syntax

   The expression syntax adopted by this module allows Prolog terms to represent
   or denote Julia expressions. Let T be the domain of recognised Prolog terms (denoted by
   type expr), and M be the domain of Julia expressions written in Julia syntax.
   Then V : T->M is the valuation function which maps Prolog term X to Julia expression V[X].
   These are some of the constructs it recognises:

   ==
   X:>:Y           % |--> V[X]; V[Y]  (sequential evaluation returning second value)
   X=Y             % |--> V[X]=V[Y] (assignment, X must denote a valid left-value)
   if(X,Y)         % |--> if V[X] then V[Y] end
   if(X,Y,Z)       % |--> if V[X] then V[Y] else V[Z] end

   +X              % |--> +(V[X])
   -X              % |--> -(V[X])
   X+Y             % |--> +(V[X],V[Y])
   X-Y             % |--> -(V[X],V[Y])
   X^Y             % |--> ^(V[X],V[Y])
   X*Y             % |--> *(V[X],V[Y])
   X/Y             % |--> /(V[X],V[Y])
   X\Y             % |--> \(V[X],V[Y])
   X.^Y            % |--> .^(V[X],V[Y])
   X.*Y            % |--> .*(V[X],V[Y])
   X./Y            % |--> ./(V[X],V[Y])
   X.\Y            % |--> .\(V[X],V[Y])
   X rdiv Y        % |--> //(V[X],V[Y])
   X:Y:Z           % |--> colon(V[X],V[Y],V[Z])
   X:Z             % |--> colon(V[X],V[Z])
   X>Z             % |--> >(V[X],V[Y])
   X>=Z            % |--> >=(V[X],V[Y])
   X<Z             % |--> <(V[X],V[Y])
   X=<Z            % |--> <=(V[X],V[Y])
   X==Z            % |--> ==(V[X],V[Y])
   [X1,X2,...]     % |--> [ V[X1], V[X2], ... ]

   :X              % |--> :V[X] (symbol or abstract syntax tree)
   'Inf'           % |--> Inf (positive infinity)
   'NaN'           % |--> NaN (not a number)
   X`              % |--> ctranpose(V[X]) (conjugate transpose, V[X]')
   X`Y             % |--> V[X].V[q(Y)]
   X\\Y            % |--> (V[X]) -> V[Y])
   q(X)            % wrap V[X] in single quotes (escaping internal quotes)
   qq(X)           % wrap V[X] in double quotes (escaping internal double quotes)
   ==

   ==
   arr(Lists)         % multidimensional array from nested lists.
   arr(Dims,Lists)    % multidimensional array from nested lists.
   int64(Dims,Lists)  % multidimensional array of Int64
   float64(Dims,Lists)% multidimensional array of Int64
   ==

   Things to bypass default formatting
   ==
   noeval(_)          % triggers a failure when processed
   \(P)               % escape and call phrase P directly to generate string
   $(X)               % calls pl2jl_hook/2, denotes V[Y] where pljl_hook(X,Y).
   '$VAR'(N)          % gets formatted as p_N where N is assumed to be atomic.
   ==

   All other Prolog atoms are written using write/1.
   Prolog dictionaries are written as Julia dictionaries. Dictionary keys can
   be atoms (written as Julia symbols) or small integers (written as Julia integers).
   Other Prolog terms are assumed to be calls to functions named according to the head functor.
   Thus V[ <head>( <arg1>, <arg2>, ...) ] = <head>(V[<arg1>, V[<arg2>], ...).

   @tbd

   Expression language: arr(Vals,Shape,InnerFunctor) - allows efficient
   representation of arrays of arbitrary things. Will require more strict
   nested list form.
*/

:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).

:- set_prolog_flag(back_quotes,symbol_char).
:- set_prolog_flag(double_quotes,codes).


%% pl2jl_hook(+I:engine,+X:term,-Y:expr) is nondet.
%% pl2jl_hook(+X:term,-Y:expr) is nondet.
%  Clauses of pl2jl_hook/2 allow for extensions to the expression
%  language such that =|V[$X] = V[Y]|= if =|pl2jl_hook(X,Y)|=.


%% expr(+X:expr)// is nondet.
%  Convert Julia expression as a Prolog term to string representation.
expr(A:>:B)      --> !, "(", expr(A), ";", expr(B), ")".
expr(A=B)        --> !, "(", expr(A), "=", expr(B), ")".
expr(if(A,B))    --> !, "if ",expr(A), " ", expr(B), " end".
expr(if(A,B,C))  --> !, "if ",expr(A), " ", expr(B), " else ", expr(C), " end".
expr(using(P))   --> !, "using ", atm(P).

expr(\X)         --> !, phrase(X).
expr($X)         --> !, {pl2jl_hook(X,Y)}, expr(Y).
expr(q(X))       --> !, q(expr(X)).
expr(qq(X))      --> !, qq(expr(X)).
expr(noeval(_))  --> !, {fail}. % causes evaluation to fail.

expr(A+B) --> !, "+", args(A,B).
expr(A-B) --> !, "-", args(A,B).
expr( -B) --> !, "-", args(B).
expr( +B) --> !, "+", args(B).
expr(A^B) --> !, "^", args(A,B).
expr(A*B) --> !, "*", args(A,B).
expr(A/B) --> !, "/", args(A,B).
expr(A\B) --> !, "\\", args(A,B).
expr(A.^B)--> !, ".^", args(A,B).
expr(A.*B)--> !, ".*", args(A,B).
expr(A./B)--> !, "./", args(A,B).
expr(A.\B)--> !, ".\\", args(A,B).
expr(A>B) --> !, ">",args(A,B).
expr(A<B) --> !, "<",args(A,B).
expr(A>=B)--> !, ">=",args(A,B).
expr(A=<B)--> !, "=<",args(A,B).
expr(A==B)--> !, "==",args(A,B).
expr(A\=B)--> !, "!=",args(A,B).
expr(\+A) --> !, "!",args(A).
expr(A:B:C) --> !, expr(colon(A,B,C)).
expr(A:B) --> !, expr(colon(A,B)).
expr(rdiv(A,B)) --> !, "//", args(A,B).
expr(A=>B)--> !, "=>",args(A,B).
expr(<<(A,B)) --> !, "∘", args(A,B). % function composition

expr([])     --> !, "[]".
expr([X|Xs]) --> !, "[", seqmap_with_sep(",",expr,[X|Xs]), "]".

expr(:B) --> !, ":", {atomic(B)} -> atm(B); ":(", expr(B), ")".
expr(A`B) --> !, expr(A), ".", atm(B).
expr(B`)  --> !, "ctranspose", args(B).
expr(B...) --> !, expr(B), "...".
expr(A\\B) --> !, { term_variables(A,V), varnames(V) },
   "((", arglist(A), ") -> ", expr(B), ")".

expr([](Xs,'`')) --> !, "[", slist(Xs), "]".
expr([](Is,X))   --> !, expr(X), "[", clist(Is), "]".
expr(A@B)        --> !, expr(A), arglist(B).
expr(A.@B)       --> !, expr(.A), arglist(B).
expr(.A)         --> !, "(", expr(A), ")", ".".
expr(#(A))       --> !, arglist([A]).
expr(#(A,B))     --> !, arglist([A,B]).
expr(#(A,B,C))   --> !, arglist([A,B,C]).
expr(#(A,B,C,D)) --> !, arglist([A,B,C,D]).

expr(int64(S,L))   --> !, "reshape(Int64[", flatten(S,L), "],reverse(", arglist(S), "))".
expr(float64(S,L)) --> !, "reshape(Float64[", flatten(S,L), "],reverse(", arglist(S), "))".

expr(arr($X))    --> !, { pl2jl_hook(X,L) }, expr(arr(L)).
expr(arr(L))     --> !, { array_dims(L,D) }, array(D,L).
expr(arr(D,L))   --> !, array(D,L).
expr(arr(D,L,P)) --> !, array(D,P,L).
expr('$VAR'(N))  --> !, "p_", atm(N).

% these are the catch-all clauses which will deal with identifiers literals function calls, and dicts
expr(A) --> {string(A)}, !, qq(str(A)).
expr(A) --> {atomic(A)}, !, atm(A).
expr(F) --> {compound_name_arity(F,H,0)}, !, atm(H), "()".
expr(A) --> {is_dict(A)}, !, {dict_pairs(A,_,Ps), maplist(pair_to_jl,Ps,Ps1)}, "Dict", arglist(Ps1).
expr(F) --> {F=..[H|AX]}, atm(H), arglist(AX).

expr_with(Lambda,Y) --> {copy_term(Lambda,Y\\PY)}, expr(PY).
pair_to_jl(K-V, KK=>V) :- atom(K) -> KK= :K; KK=K.


% dimensions implicit in nested list representation
array_dims([X|_],M) :- !, array_dims(X,N), M is N+1.
array_dims(_,0).

flatten([], X) --> expr(X).
flatten([_|D], L) --> seqmap_with_sep(",", flatten(D), L).

%% array(+Dims:natural, +Id:ml_eng, +Array)// is det.
%
%  Format nested lists as a multidimensional array.
%  Dims is the number of dimensions of the resulting array and
%  should equal the nesting level of Array, ie if Array=[1,2,3],
%  Dims=1; if Array=[[1,2],[3,4]], Dims=2, etc.
array(0,X) --> !, expr(X).
array(1,L) --> !, "[", seqmap_with_sep(";",expr,L), "]".
array(2,L) --> !, "[", seqmap_with_sep(" ",array(1),L), "]".
array(N,L) --> {succ(M,N)}, "cat(", atm(N), ",", seqmap_with_sep(",",array(M),L), ")".

array(0,P,X) --> !, expr_with(P,X).
array(1,P,L) --> !, "[", seqmap_with_sep(";",expr_with(P),L), "]".
array(2,P,L) --> !, "[", seqmap_with_sep(" ",array(1,P),L), "]".
array(N,P,L) --> {succ(M,N)}, "cat(", atm(N), ",", seqmap_with_sep(",",array(M,P),L), ")".

%% clist(+Id:ml_eng, +Items:list(expr))// is det.
%  Format list of Julia expressions in a comma separated list.
clist([]) --> [].
clist([L1|LX])  --> expr(L1), seqmap(do_then_call(",",expr),LX).

slist([]) --> [].
slist([L1|LX])  --> expr(L1), seqmap(do_then_call(" ",expr),LX).

%% arglist(+Id:ml_eng, +Args:list(expr))// is det.
%  DCG rule to format a list of Julia expressions as function arguments
%  including parentheses.
arglist(X) --> "(", clist(X), ")".

%% args(+Id:ml_eng, +A1:expr, +A2:expr)// is det.
%% args(+Id:ml_eng, +A1:expr)// is det.
%
%  DCG rule to format one or two Julia expressions as function arguments
%  including parentheses.
args(X,Y) --> "(", expr(X), ",", expr(Y), ")".
args(X) --> "(", expr(X), ")".

%% atm(+A:atom)// is det.
%  DCG rule to format an atom using write/1.
atm(A,C,T) :- format(codes(C,T),'~w',[A]).

varnames(L) :- varnames(1,L).
varnames(_,[]).
varnames(N,[TN|Rest]) :-
   atom_concat(p_,N,TN), succ(N,M),
   varnames(M,Rest).


%% term_jlstring(+X:expr,-Y:list(code)) is det.
%  Convert term representing Julia expression to a list of character codes.
term_jlstring(Term,String) :- phrase(expr(Term),String), !.
