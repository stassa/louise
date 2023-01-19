:-module(even_odd, [background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ,zero/1
                   ,prev/2
                   ]).

:-use_module(configuration).

/** <module> Learn "even" by inventing "odd".

This experiment file shows how to learn "even" by inventing "odd" from a
single positive and a single negative example of "even".

1. Known good configuration:

To run this experiment, make sure that configuration options are as
listed below. Important options are marked with "*".

==
?- list_config.
* clause_limit(3)
example_clauses(call)
* experiment_file(data/examples/even_odd.pl,even_odd)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
listing_limit(10)
* max_error(0,0)
* max_invented(1)
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

2. Elements of the MIL problem.

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


3. Make a learning query:

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
