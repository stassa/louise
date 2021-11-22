:-module(findlast, [background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ]).

/** <module> One-shot learning of a recursive theory.

This experiment was proposed by Andrew Cropper.

The following is an example of one-shot learning of a recursive theory
where the single example is not an example of the base-case (a.k.a. the
terminating or boundary condition).

The example uses the metarule meta-interpreter resolve_metarules/5 that
proves positive examples by resolution with metarules and clauses added
to the Top Program during learning, in addition to examples and
background knowledge. Resolving with the metarules allows the derivation
of clauses that self-resolve. Resolving with clauses in the Top Program
allows the derivation of clauses that depend on each other, such as
mutually recursive clauses, or, in this case, a recursive claus and its
base-case.

The metarule meta-interpreter resolve-metarules/5 is still in
development and so its use, or not, is controlled by the setting of the
configuration option prove_recursive/1. The necessary settings are
shown below.

1. Known good configuration:
==
?- list_config.
depth_limits(2,1)
example_clauses(call)
experiment_file(data/examples/findlast.pl,findlast)
generalise_learned_metarules(false)
learner(louise)
max_invented(1)
metarule_formatting(quantified)
metarule_learning_limits(none)
minimal_program_size(2,inf)
prove_recursive(examples)
prove_recursive(others)
prove_recursive(self)
prove_recursive(top_program)
recursion_depth_limit(dynamic_learning,none)
recursion_depth_limit(metasubstitution,none)
recursive_reduction(false)
reduce_learned_metarules(false)
reduction(plotkins)
resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.
==

Make sure in particular that the following options are set:
==
prove_recursive(examples).
prove_recursive(top_program).
prove_recursive(self).
prove_recursive(others).
==

Currently these must be set by hand in configuration.pl. In particular,
they cannot be set through the auxiliary predicate
set_configuration_option/2!


2. MIL problem elements:
==
?- list_mil_problem(list_last/2).
Positive examples
-----------------
list_last([a,b,c,d,e,f,g,h,i],i).

Negative examples
-----------------
[]

Background knowledge
--------------------
tail/2:
tail([A|B],B).

head/2:
head([A|B],A).

empty/1:
empty([]).

Metarules
---------
(Tailrec-1) ∃.P,Q ∀.x,y,z: P(x,y)← Q(x,z),P(z,y)
(Midcon) ∃.P,Q,R,S ∀.x,y,z: P(x,y)← Q(x,z),R(z),S(x,y)
true.
==


3. Learning query:
==
?- learn(list_last/2).
list_last([a,b,c,d,e,f,g,h,i],i).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
true.
==

The example atom list_last([a,b,c,d,e,f,g,h,i],i) is output along with
the clauses of the learned hypothesis because the implementation of
Plotkin's reduction algorithm doesn't currently handle lists well.

If that is annoying you, set the configuration option reduction/1 to
"none" which results atomic residue to be dropped:

==
?- learn(list_last/2), configuration:reduction(R).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
R = none.
==

4. See also data/examples/example_invention.pl and
data/examples/yamamoto.pl for more examples (and discussion) of using
Louise's metarule meta-interpreter for one-shot learning.

*/


:-use_module(configuration).

configuration: tailrec_1 metarule 'P(x,y):- Q(x,z),P(z,y)'.
configuration: midcon metarule 'P(x,y):- Q(x,z),R(z),S(x,y)'.

background_knowledge(list_last/2, [tail/2,head/2,empty/1]).

metarules(list_last/2,[tailrec_1,midcon]).

positive_example(list_last/2,list_last([a,b,c,d,e,f,g,h,i],i)).

negative_example(list_last/2,_):-
        fail.

empty([]).
head([H|_],H).
tail([_|T],T).

