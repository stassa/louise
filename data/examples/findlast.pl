:-module(findlast, [background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ,empty/1
                   ,head/2
                   ,tail/2
                   ]).

:-use_module(configuration).

/** <module> One-shot learning of a recursive theory.

This experiment was proposed by Andrew Cropper.

The following is an example of one-shot learning of a recursive theory
where the single example is not an example of the base-case (a.k.a. the
terminating or boundary condition).

The example uses the metarule meta-interpreter resolve_metarules/6 that
proves positive examples by resolution between metarules, in addition to
background knowledge. Resolving between metarules allows the derivation
of clauses that depend on each other but are not known in advance, such
as a recursive clause and its base-case.

The use of the metarule meta-interpreter resolve-metarules/6 is
controlled by the setting of the configuration option prove_recursive/1.
The necessary settings are shown below.

1. Known good configuration.

The results listed further in this documentation section were obtained
with the following configuration options defined in configuration.pl:
==
?- list_config.
depth_limits(2,1)
example_clauses(call)
experiment_file(data/examples/findlast.pl,findlast)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
max_invented(1)
metarule_formatting(user_friendly)
metarule_learning_limits(none)
minimal_program_size(2,inf)
prove_recursive(others)
prove_recursive(self)
recursion_depth_limit(dynamic_learning,none)
recursion_depth_limit(metasubstitution,none)
recursive_reduction(false)
reduce_learned_metarules(false)
reduction(plotkins)
resolutions(5000)
test_constraints(proof)
theorem_prover(resolution)
unfold_invented(false)
true.
==

If the output you see looks different adjust the configuration to match
the listing above. Make sure in particular that the following options
are set:
==
prove_recursive(others).
prove_recursive(self).
==

The option prove_recursive(self) allows a metarule to resolve with
itself and thus derive clauses that can only resolve with copies of
themselves, or other instances of the same metarule. We call this
self-resolution.

The option prove_recursive(others) allows a metarule to resolve with
other metarules and thus allows the derivation of clauses that are
instances of different metarules. We call this all-resolution.

Currently these options must be set by hand in the main configuration
file, configuration.pl. In particular, they cannot be set through the
auxiliary predicate set_configuration_option/2!


2. MIL problem elements.

The output of the experiments listed further in this documentation
section was generated with the following MIL problem elements:
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
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
(Midcon) ∃.P,Q,R,S ∀.x,y,z: P(x,y)← Q(x,z),R(z),S(x,y)
true.
==

As with configuration, if the output you see looks different make sure
you have the correct problem specification.


3. Learning query.

Call the learning predicate learn/1:
==
?- learn(list_last/2).
list_last([a,b,c,d,e,f,g,h,i],i).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
true.
==

The learned program finds the last element of a list in its first
argument and binds it to its second argument. Note that this differs
from the canonical definition of last/2 in Prolog:
==
last(Xs,X).
last([_|Xs],X):-
        last(Xs,X).
==

The difference is that the list_last/2 program use the predicates
tail/2, head/2 and empty/1 to replace the use of the list-construction
operator [|]. The metarules used by Louise are _datalog_ which means
they can include no function symbols. In order to instantiate a variable
to a list, an auxiliary predicate like head/2 must be used. This is
called flattening.

Note that the example atom list_last([a,b,c,d,e,f,g,h,i],i) is output
along with the clauses of the learned hypothesis because the
implementation of Plotkin's reduction algorithm doesn't currently handle
lists well.

If that is annoying you, set the configuration option reduction/1 to
"none" which results in atomic residue to be dropped:

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

configuration:midcon metarule 'P(x,y):- Q(x,z),R(z),S(x,y)'.

background_knowledge(list_last/2, [tail/2,head/2,empty/1]).

metarules(list_last/2,[chain,midcon]).

positive_example(list_last/2,list_last([a,b,c,d,e,f,g,h,i],i)).

negative_example(list_last/2,_):-
        fail.

empty([]).
head([H|_],H).
tail([_|T],T).
