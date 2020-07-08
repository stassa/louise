:-module(incremental_refinmnt, [background_knowledge/2
			       ,metarules/2
			       ,positive_example/2
			       ,negative_example/2
			       ,edge/2
			       ,path/2
			       ]).

/** <module> Example of predicate invention by incremental refinement.

Usage instructions
------------------

1. Ensure the following constraint is uncommented in the source code
below:

==
metarule_constraints(M,fail):-
	M =.. [m,Id,P|Ps]
	,Id \= projection
	,left_recursive(P,Ps).

left_recursive(T,[T|_Ps]):-
	!.
left_recursive(T,[T,T|_Ps]):-
	!.
left_recursive(T,[I,T|_Ps]):-
	atom_chars(I,['$',A])
	,atom_number(A,_N).
==

This will significantly restrict the number of clauses in invented
predicates for this example (but not necessarily for other examples,
i.e. don't assume you always need to prohibit left-recursion when
performing incremental refinement).


2. Ensure that the symbol of the target predicate, path/2 is added to
the background knowledge:

==
background_knowledge(path/2,[path/2,edge/2]).
==

This will allow path/2 to be used in the definitions of any predicates
invented by incremental refinement.


3. Use incremental_refinement/5 to invent one or more predicates from
the elements of the MIL problem for path/2 defined in this experiment
file. In the examples below, K is the maximum number of predicates to
attempt to invent:

==
?- _K = 1, experiment_data(path/2,_Pos,_Neg,_BK,_MS), incremental_refinement(_K,_Pos,_Neg,_BK,_MS,_Is),print_clauses(_Is).
'$1'(A,B):-edge(A,B).
'$1'(A,B):-path(A,B).
'$1'(A,B):-edge(A,C),edge(C,B).
true.

?- _K = 3, experiment_data(path/2,_Pos,_Neg,_BK,_MS), incremental_refinement(_K,_Pos,_Neg,_BK,_MS,_Is),print_clauses(_Is).
'$3'(A,B):-'$2'(A,B).
'$3'(A,B):-'$2'(A,C),'$2'(C,B).
'$2'(A,B):-'$1'(A,B).
'$2'(A,B):-'$1'(A,C),'$1'(C,B).
'$1'(A,B):-edge(A,B).
'$1'(A,B):-path(A,B).
'$1'(A,B):-edge(A,C),edge(C,B).
true.

?- _K = 5, experiment_data(path/2,_Pos,_Neg,_BK,_MS), incremental_refinement(_K,_Pos,_Neg,_BK,_MS,_Is),print_clauses(_Is).
'$5'(A,B):-'$4'(A,B).
'$5'(A,B):-'$4'(A,C),'$4'(C,B).
'$4'(A,B):-'$3'(A,B).
'$4'(A,B):-'$3'(A,C),'$3'(C,B).
'$3'(A,B):-'$2'(A,B).
'$3'(A,B):-'$2'(A,C),'$2'(C,B).
'$2'(A,B):-'$1'(A,B).
'$2'(A,B):-'$1'(A,C),'$1'(C,B).
'$1'(A,B):-edge(A,B).
'$1'(A,B):-path(A,B).
'$1'(A,B):-edge(A,C),edge(C,B).
true.
==


4. Use learn_incremental/[3,5] to perform predicate invention by
incremental refinement and learn a hypothesis reusing the predicates
invented in the first step, possibly invented more new predicates:

==
?- _K = 1, _J = 0, _BK2 = append, experiment_data(path/2,_Pos,_Neg,_BK,_MS),learn_incremental([_Pos,_Neg,_BK,_MS,_K],[_Pos,_Neg,_BK2,_MS,_J],_Is,_Ps),print_clauses('Invented:',_Is), nl, print_clauses('Learned:',_Ps).
Invented:
'$1'(A,B):-edge(A,B).
'$1'(A,B):-path(A,B).
'$1'(A,B):-edge(A,C),edge(C,B).

Learned:
path(a,z).
'$1'(A,B):-path(A,B).
'$1'(A,B):-edge(A,B).
'$1'(A,B):-edge(A,C),edge(C,B).
path(A,B):-'$1'(A,B).
true.
==

See incremental_refinement/5 and learn_incremental/[3,5] for more
examples of use and explanations of incremental refinement.

*/

configuration:metarule_constraints(M,fail):-
	M =.. [m,Id,P|Ps]
	,Id \= projection
	,left_recursive(P,Ps).

left_recursive(T,[T|_Ps]):-
	!.
left_recursive(T,[T,T|_Ps]):-
	!.
left_recursive(T,[I,T|_Ps]):-
	atom_chars(I,['$',A])
	,atom_number(A,_N).


background_knowledge(path/2,[path/2,edge/2]).

metarules(path/2,[chain,identity]).

positive_example(path/2,path(X,Y)):-
	path(X,Y).

negative_example(path/2,_):-
	fail.

path(a,z).

edge(a,b).
edge(b,c).
edge(c,d).
edge(d,e).
edge(e,f).
edge(f,g).
edge(g,h).
edge(h,i).
edge(i,j).
edge(j,k).
edge(k,l).
edge(l,m).
edge(m,n).
edge(n,o).
edge(o,p).
edge(p,q).
edge(q,r).
edge(r,s).
edge(s,t).
edge(t,u).
edge(u,v).
edge(v,w).
edge(w,x).
edge(x,y).
edge(y,z).
