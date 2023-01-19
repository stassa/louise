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
* clause_limit(0)
example_clauses(call)
* experiment_file(data/examples/recursive_folding.pl,recursive_folding)
* fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
listing_limit(10)
* max_error(0,0)
* max_invented(0)
metarule_formatting(quantified)
metarule_learning_limits(none)
minimal_program_size(2,inf)
recursive_reduction(false)
reduce_learned_metarules(false)
* reduction(plotkins)
* resolutions(5000)
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

4. First learning attempt with fold_recursive/1 set to 'false':
==
?- learn(list_last/2), fold_recursive(F).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),tail(C,D),empty(D),head(C,B).
list_last(A,B):-tail(A,C),tail(C,D),tail(D,E),empty(E),head(D,B).
F = false.
==

5. The elements of the MIL problem listed in step 3 above are designed
to lead to an over-specialised hypothesis as in step 4. Setting the
configuration option fold_recursive/1 to 'true' will cause Louise to
fold the recursive hypothesis at the end of learning to attempt to
introduce recursion:
==
?- learn(list_last/2), fold_recursive(F).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
list_last(A,B):-tail(A,C),list_last(C,B).
F = true.
==

6. Add the recursive hypothesis to a source file and load it, then test
it to verify it's a correct hypothesis:
==
?- _Ls = [1,2,3,4,5], recursive_folding:list_last(_Ls, L).
L = 5 ;
false.
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
