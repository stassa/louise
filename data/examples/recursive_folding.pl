:-module(recursive_folding, [background_knowledge/2
                            ,metarules/2
                            ,positive_example/2
                            ,negative_example/2
                            ,head/2
                            ,tail/2
                            ,empty/1
                            ]).

:-use_module(configuration).

/** <module> Post-process an over-special hypothesis to introduce recursion.

1. Known good configuration:
==
?- list_config.
example_clauses(call)
experiment_file(data/examples/recursive_folding.pl,recursive_folding)
generalise_learned_metarules(false)
generalised_examples(fully)
learned_metarules_printing(pretty)
learner(louise)
max_invented(1)
metarule_learning_limits(metasubstitutions(1))
minimal_program_size(2,inf)
recursion_depth_limit(dynamic_learning,none)
recursive_reduction(false)
reduce_learned_metarules(false)
reduction(plotkins)
resolutions(5000)
symbol_range(predicate,[P,Q,R,S,T])
symbol_range(variable,[X,Y,Z,U,V,W])
theorem_prover(resolution)
unfold_invented(false)
true.
==

2. MIL problem elements:
==
?- list_mil_problem(list_last/2).
Positive examples
-----------------
list_last([a],a).
list_last([1,2],2).
list_last([a1,b2,c3],c3).

Negative examples
-----------------
[]

Background knowledge
--------------------
head/2:
head([A|B],A).

tail/2:
tail([A|B],B).

empty/1:
empty([]).

Metarules
---------
(M1) ∃.P,Q,R,S ∀.x,y,z: P(x,y)← Q(x,z),R(z),S(x,y)
(M2) ∃.P,Q,R,S,T ∀.x,y,z,u: P(x,y)← Q(x,z),R(z,u),S(u),T(z,y)
(M3) ∃.P,Q,R,S,T,P1 ∀.x,y,z,u,v: P(x,y)← Q(x,z),R(z,u),S(u,v),T(v),P1(u,y)
true.
==

4. First learning attempt:
==
?- learn(list_last/2).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
list_last(A,B):-tail(A,C),tail(C,D),tail(D,E),empty(E),head(D,B).
true.
==

5. An over-specialised hypothesis is learned from the elements of the
MIL problem in 3. Now, post-process the hypothesis to turn it into a
recursive program:
==
?- learn(list_last/2, _Ps), print_clauses('Learned', _Ps), fold_recursive(_Ps, _Fs), nl, print_clauses('Folded', _Fs).
Learned
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
list_last(A,B):-tail(A,C),tail(C,D),tail(D,E),empty(E),head(D,B).

Folded
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
list_last(A,B):-tail(A,C),tail(C,D),list_last(D,B).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-list_last(A,B).
true.
==

6. Add the recursive hypothesis to a source file and load it, then test
it:
==
?- _Ls = [1,2,3,4,5], list_last:list_last(_Ls, L).
L = 5 .
==

Note that the folded, recursive program works correctly, although it
risks entering an infinite recursion if you ask for more results after
the first because of its left-recursive last clause:
==
list_last(A,B):-list_last(A,B).
==

7. The left-recursive last clause (and other redundant clauses) can be
removed by Plotkin's program reduction algorithm:
==
?- learn(list_last/2, _Ps), print_clauses('Learned', _Ps), fold_recursive(_Ps, _Fs), nl, print_clauses('Folded', _Fs), nl, program_reduction(_Fs,_Rs,_Ds), print_clauses('Reduced', _Rs), nl, print_clauses('Redundant:',_Ds).
Learned
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
list_last(A,B):-tail(A,C),tail(C,D),tail(D,E),empty(E),head(D,B).

Folded
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
list_last(A,B):-tail(A,C),tail(C,D),list_last(D,B).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-list_last(A,B).

Reduced
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
list_last(A,B):-tail(A,C),tail(C,D),list_last(D,B).
list_last(A,B):-tail(A,C),list_last(C,B).

Redundant:
list_last(A,B):-list_last(A,B).
true.
==

*/

configuration:m1 metarule 'P(x,y):- Q(x,z),R(z),S(x,y)'.
configuration:m2 metarule 'P(x,y):- Q(x,z),R(z,v),S(v),T(z,y)'.
configuration:m3 metarule 'P(x,y):- Q(x,z),R(z,v),S(v,w),T(w),U(v,y)'.

background_knowledge(list_last/2, [head/2, tail/2, empty/1]).

metarules(list_last/2,[m1,m2,m3]).

positive_example(list_last/2,list_last([a],a)).
positive_example(list_last/2,list_last([1,2],2)).
positive_example(list_last/2,list_last([a1,b2,c3],c3)).

negative_example(list_last/2,_):- false.

head([H|_T], H).
tail([_H|T],T).
empty([]).

/* Recursive hypothesis
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
list_last(A,B):-tail(A,C),tail(C,D),list_last(D,B).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-list_last(A,B).
*/
