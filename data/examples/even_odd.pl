:-module(even_odd, [background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ,zero/1
                   ,prev/2
                   ]).

:-use_module(project_root(configuration)).

/** <module> Learn "even" by inventing "odd".

This experiment file shows how to learn "even" by inventing "odd" from a
single positive and a single negative example of "even".

__1. Known good configuration__

To run this experiment, make sure that configuration options include the
ones listed below:

==
?- _Options = [experiment_file/2, clause_limit/1, fetch_clauses/1, max_invented/1, max_error/2, reduction/1, resolutions/1], nl, list_options(_Options).

experiment_file(data/examples/even_odd.pl,even_odd)
clause_limit(3)
fetch_clauses(all)
max_invented(1)
max_error(0,0)
reduction(plotkins)
resolutions(5000)
true.
==

__2. Elements of the MIL problem__

Make sure that the MIL problem elements defined in this file match the
ones listed below:

==
?- list_mil_problem(even/1).
Positive examples
-----------------
even(4).

Negative examples
-----------------
:-even(3).

Background knowledge
--------------------
zero/1:
zero(0).

prev/2:
prev(1,0).
prev(2,1).
prev(3,2).
prev(4,3).

Metarules
---------
(M1) ∃.P,Q ∀x: P(x)← Q(x)
(M2) ∃.P,Q,R ∀.x,y: P(x)← Q(x,y),R(y)

true.
==


__3. Make a learning query__

==
?- learn(even/1).
even(A):-zero(A).
'$1'(A):-prev(A,B),even(B).
even(A):-prev(A,B),'$1'(B).
true.
==

Note that the invented predicate '$1' reprsents the concept of odd
parity.


*/

:- auxiliaries:set_configuration_option(clause_limit, [3]).
:- auxiliaries:set_configuration_option(max_invented, [1]).
:- auxiliaries:set_configuration_option(reduction, [plotkins]).

configuration: m1 metarule 'P(x):- Q(x)'.
configuration: m2 metarule 'P(x):- Q(x,y), R(y)'.

background_knowledge(even/1, [zero/1, prev/2]).

metarules(even/1,[m1,m2]).

positive_example(even/1,even(4)).

negative_example(even/1,even(3)).
%negative_example(even/1,_):- fail. %+

zero(0).

prev(1,0).
prev(2,1).
prev(3,2).
prev(4,3).
