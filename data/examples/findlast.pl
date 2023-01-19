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

1. Introduction

The example in this experiment file demonstrates one-shot learning of a
recursive theory. The learning target is a program that finds the last
element of a list.

A single positive example is given, which is an example of the inductive
case. Since no example of the base-case is given, Louise must derive the
base-case on its own.

This is a hard task that Meta-Interpretive Learning (MIL) systems like
Louise can accomplish because they learn by completing a proof of their
examples by SLD-resolution. Resolution works by deriving intermediary
goals, and when the target theory is recursive the last goal derived is
the (complement of the) base-case, terminating the recursion.

Moreover, because MIL systems learn by SLD-resolution, given inductively
sufficient background knowledge and metarules, a MIL system can always
learn a set of clauses that entail a single example. An informal
explanation of this ability is that resolution does not need more than a
single goal atom (a single, negative literal) to complete a proof. In
MIL, goals are given as positive or negative examples, so a single
example is always enough to complete a proof. Whether the proof is
recursive or not makes no difference.


1. Known good configuration.

The results listed further in this documentation section were obtained
with the following configuration options. Notable options are marked
with "*":

==
?- list_config.
* clause_limit(2)
example_clauses(call)
* experiment_file(data/examples/findlast.pl,findlast)
fold_recursive(false)
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
* reduction(none)
* resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.
==

If the output you see when you run the experiment looks different than
the results listed later in this comment section, adjust the
configuration to match the listing above. Make sure in particular that
the following option is set:

==
clause_limit(2)
==

The option clause_limit(2) tells Louise to try and learn up to 2 clauses
from the single positive example. This is necessary because the (known)
target theory for this experiment is a recursive program with one
inductive case and one base-case that must be learned together.

This option can be set in the configuration, or by calling the auxiliary
predicate set_configuration_option/2, which modifies the configuration
options already loaded in memory. You can find the appropriate call to
set_configuration_option/2 declared as a directive in this experiment
file, after this comment section:

==
:- auxiliaries:set_configuration_option(clause_limit, [2]).
==


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
(Tailrec) ∃.P,Q ∀.x,y,z: P(x,y)← Q(x,z),P(z,y)
(Midcon) ∃.P,Q,R,S ∀.x,y,z: P(x,y)← Q(x,z),R(z),S(x,y)

true.
==

As with configuration, if the output you see looks different make sure
you have the correct MIL problem elements.


3. Learning query.

Tell Louise to learn a hypothesis from the elements of the MIL problem
with a call to learn/1, as shown below:

==
?- learn(list_last/2).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
true.
==

The learned program finds the laste element of its input list by
"walking over" the list.

Note that the learned program differs from the canonical definition of
last/2 in Prolog:

==
last(Xs,X).
last([_|Xs],X):-
        last(Xs,X).
==

In truth, the two programs are success-set equivalent. The difference is
that the list_last/2 program uses the background predicates tail/2,
head/2 and empty/1 instead of the list-construction operator, [|].

The reason for this is that metarules used by Louise are _datalog_ which
means they can include no function symbols. In Prolog's syntax functions
correspond to "compound terms" given as arguments to predicates. [|] is
one such.

Because of the restriction of metarules to datalog, in order to
instantiate a variable to a list, an auxiliary predicate like head/2
must be used. The use of auxiliary predicates in place of functions is
called flattening in ILP.


4. Program reduction

The experiment file source code below includes a directive to set the
value of the configuration option reduction/2 to "none":

==
:- auxiliaries:set_configuration_option(reduction, [none]).
==

Without this directive, and with the default-ish reduction(plotkins)
option, the output of learn/1 includes the single positive example:

==
?- learn(list_last/2), configuration:reduction(R).
list_last([a,b,c,d,e,f,g,h,i],i).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
R = plotkins.
==

This happens because the implementation of Plotkin's program
reduction algorithm in Louise doesn't work well when examples include
lists. With reduction(plotkins) any positive examples that the program
reduction code can't prove are entailed by the learned program are
output together with the clauses of the program. With reduction(none)
no examples are output with the learned program. So we set
reduction(none) to eliminate the "atomic residue" from the output:

==
?- learn(list_last/2), configuration:reduction(R).
list_last(A,B):-tail(A,C),list_last(C,B).
list_last(A,B):-tail(A,C),empty(C),head(A,B).
R = none.
==

In some cases atomic residue can be eliminated by increasing the value
of the configuration option resolutions/1 or by setting the
configuration option recursive_reduction/1 to "true". These options
control the "strength" of the reduction. But they don't really help with
examples that include lists. A better implementation of program
reduction is in the works.

*/

:- auxiliaries:set_configuration_option(clause_limit, [2]).
:- auxiliaries:set_configuration_option(reduction, [none]).

configuration:midcon metarule 'P(x,y):- Q(x,z),R(z),S(x,y)'.

background_knowledge(list_last/2, [tail/2,head/2,empty/1]).

metarules(list_last/2,[tailrec,midcon]).

positive_example(list_last/2,list_last([a,b,c,d,e,f,g,h,i],i)).

negative_example(list_last/2,_):-
        fail.

empty([]).
head([H|_],H).
tail([_|T],T).
