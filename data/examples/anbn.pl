:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,a/2
	       ,b/2
	       ]).

/** <module> Learn an a^nb^n CFG by predicate invention.

The example in this experiment file llustrates the use of predicate
invention to learn a recursive theory; the use of unfolding to remove
invented predicates from a learned hypothesis; and the use of
metasubstitution constraints to avoid adding unwanted clauses to the
learned hypothesis.

1. Known good configuration.

The results listed below were obtained with the following configuration
options defined in configuration.pl:
==
?- list_config.
depth_limits(3,1)
example_clauses(call)
experiment_file(data/examples/anbn.pl,anbn)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
max_invented(1)
metarule_formatting(user_friendly)
metarule_learning_limits(none)
minimal_program_size(2,inf)
prove_recursive(invented)
prove_recursive(top_program)
recursion_depth_limit(dynamic_learning,none)
recursion_depth_limit(metasubstitution,none)
recursive_reduction(false)
reduce_learned_metarules(false)
reduction(plotkins)
resolutions(5000)
test_constraints(clause)
theorem_prover(resolution)
unfold_invented(false)
true.
==

If the output you see looks different adjust the configuration to match
the listing above.

In particular, make sure that the following options are set:
==
max_invented(1)
prove_recursive(invented)
prove_recursive(top_program)
==

The option max_invented(1) tells Louise to attempt to define at least
one predicate. You can set this option in the configuration file or by
means of the set_configuration_option/2 predicate, called as a directive
in this experiment file:
==
:- auxiliaries:set_configuration_option(max_invented, [1]).
==

The option prove_recursive(top_program) tells Louise to add each clause
in the Top Program to the background knowledge so that they can be
resolved with new candidate clauses. Each clause in the Top Program is
added to the background knowledge as soon as that clause is derived.

Finally, the option prove_recursive(invented) tells Louise to try and
derive new clauses by resolution with clauses of invented predicates.
This is the option that activates predicate invention, proper.


2. MIL Problem elements.

Ensure also that the output of list_mil_problem/2 matches the following,
to make sure the elements of the MIL problem are as expected:

==
?- list_mil_problem(s/2).
Positive examples
-----------------
s([a,b],[]).
s([a,a,b,b],[]).

Negative examples
-----------------
[]

Background knowledge
--------------------
a/2:
a([a|A],A).

b/2:
b([b|A],A).

Metarules
---------
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
true.
==


3. Make a first learning attempt.
==
?- learn(s/2).
'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-a(A,C),s(C,B).
'$1'(A,B):-b(A,C),b(C,B).
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-'$1'(A,C),'$1'(C,B).
s(A,B):-'$1'(A,C),b(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).
true.
==

In the learned program output above, the predicate '$1'/2 is invented,
meaning that its definition is not given as background knowledge,
neither are training examples of it given by the user. '$1'/2 is learned
in the process of learning the target predicate s/2.


4. Eyballing results.

The learned hypothesis is the Top Program for the MIL problem defined in
this experiment file. The Top Program is the set of all clauses that
entail at least one positive and exactly zero negative examples with
respect to background knowledge. In the output of learn/1 above we can
distinguish multiple sub-programs of the Top Progam:
==
% Sub-program 1
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).

% Sub-program 2
'$1'(A,B):-a(A,C),s(C,B).
s(A,B):-'$1'(A,C),b(C,B).
s(A,B):-a(A,C),b(C,B).

% Sub-program 3
'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-b(A,C),b(C,B).
s(A,B):-'$1'(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).
==

Sub-program 1 and Sub-program 2 are both equivalent to the target
theory, a grammar of the anbn language in Definite Clause Grammar form.
Sub-program 3 is an over-specialisation that only covers the n=2
example (i.e. the string aabb).


5. Unfolding to remove invented predicates.

Set the configuration option unfold_invented/1 to "true" to unfold
the program learned in the previous step:
==
% unfold_invented(true).

?- learn(s/2).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,D),b(D,B).
true.
==

Unfolding removes clauses of invented predicates from the learned
program and replaces them with equivalent clauses.

The first clause in the unfolded program is the base-case, covering anbn
strings where n=1. This clause is not affected by unfolding because it
has no literals with invented predicate symbols.

The recursive clause is a three-literal clause that is equivalent to the
first and second clause in both Sub-program 1 and Sub-program 2. The
difference is only in the order in which the invented literal appears in
the body of these two sub-programs. There are many ways to skin that
cat! Top Program Construction by default learns them all.

Note that in the unfolded program above, there is no clause
equivalent to the clauses in Sub-program 3 (other than the base-case).
This is because, by default, Louise applies program reduction to an
unfolded program. Reduction of the unfolded program is controlled by the
setting of the configuration option reduction/1. This is the same
configuration option that constrols the reduction of the Top Program
right after learning (and before unfolding). There is no option to
reduce the Top Program only before or only after unfolding!


6. Weakenig reduction.

To inhibit reduction and reveal the unfolded clause that is
equivalent to Sub-program 3 in the learned Top Program, set the
configuration option reduction/1 to "none":
==
% reduction(none).

?- learn(s/2).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),a(C,D),b(D,E),b(E,B).
s(A,B):-a(A,C),s(C,D),b(D,B).
true.
==

The second clause in the unfolded, but unreduced, Top Program printed
above is an over-specialisation in the sense that it only covers the n=2
example (the string aabb). That clause is also redundant given the other
two clauses since those two clauses together suffice to cover the n=2
example. Hence that clause is removed by program reduction.


7. Use of constraints.

Metasubstitution constraints can be used to shape the Top Program to
your needs, for example to remove unwanted clauses from the learned
program.

Uncomment the following constraints, defined in the experiment file
source below:
==
configuration:metarule_constraints(m(chain,_,'$1',_),fail).
configuration:metarule_constraints(m(chain,'$1',a,s),fail).
==

Make a new learning attempt. First, set unfold_invented/1 to "false" to
see which clauses are included in the Top Program:
==
?- learn(s/2).
'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-b(A,C),b(C,B).
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).
true.
==

The metasubstitution constraints we defined stop the clauses of
Sub-program 2 from being added to the Top Program. The clauses of
Sub-program 1 are not affected so unfolding the learned hypothesis still
returns the target theory:
==
?- learn(s/2).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,D),b(D,B).
true.
==

Applying metasubstitution constraints to eliminate the other two
sub-programs is left as an exercise to the reader.


8. Learning dependent clauses.

In the examples above we've set the option prove_recursive/1 to
"top_program" (and "invented"). Setting prove_recursive/1 to
"top_program" causes the Top Program Construction implementation in
Louise to add newly learned clauses to the background knowledge, so that
they can be reused in subsequent learning steps to learn clauses that
"depend" on them in the sense that the new clauses call the old
clauses recursively.

In particular, we want to add the base-case clause,
"s(A,B):-a(A,C),b(C,B)", to the background knowledge so that subsequent
learning steps can re-use it to terminate the recursion of other clauses
in the Top Program. Without the base-case recursive clauses will fail
when they reach the end of a proof tree (because of the structure of the
proof tree in this case recursion without a base-case won't go infinite-
just fail).

Let's see what happens if learned clauses aren't added to the Top
Program. Unset the configuration option prove_recursive(top_program).
The only clause of prove_recursive/1 in the configuration should be
prove_recursive(invented):

==
% With unfold_invented(false)
?- forall(prove_recursive(P),writeln(P)), nl, learn(s/2).
invented

'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-b(A,C),b(C,B).
s(A,B):-'$1'(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).
true.

% With unfold_invented(true)
?- forall(prove_recursive(P),writeln(P)), nl, learn(s/2).
invented

s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),a(C,D),b(D,E),b(E,B).
true.
==

Above we show the learned Top Program both before and after unfolding,
to make it clear what exactly is included in it.

This time the only clauses that are learned are the clauses of
Sub-program 3. While the base-case clause is learned and added to the
Top Program, it is not also added to the background knowledge and so
cannot be reused to learn the recursive clauses in Sub-program 1 and
Sub-program 2 that depend on the base-case.

Other prove_recursive/1 options like prove_recursive(self) and
prove_recursive(other) are not necessary in this case. They are, for
this MIL problem in particular, informally speaking "subsumed" by the
prove_recursive(invented) option.


9. Ordering of examples.

The current implementation of predicate invention in Louise is sensitive
to the order of training examples. Suppose we rearrange the positive
examples so that the example of the base-case comes second:
==
positive_example(s/2,E):-
	member(E, [s([a,a,b,b],[])
		  ,s([a,b],[])
		  ]).


?- list_mil_problem(s/2).
Positive examples
-----------------
s([a,a,b,b],[]).
s([a,b],[]).

Negative examples
-----------------
[]

Background knowledge
--------------------
a/2:
a([a|A],A).

b/2:
b([b|A],A).

Metarules
---------
(Chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)
true.
==

Now, even with prove_recursive(top_program), the TPC implementation
can't learn the recursive clauses in the target theory:
==
?- forall(prove_recursive(P),writeln(P)), nl, learn(s/2).
top_program
invented

s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),a(C,D),b(D,E),b(E,B).
true.
==

The recursive clauses in Sub-program 1 and Sub-program 2 can only be
learned from the example where n=2. However, these clauses depend on the
base-case that can only be learned from the example where n=1. By
ordering the n=2 example before the n=1 example we prevent Louise from
learning the base-case before learning the recursive clauses that depend
on it. Therefore, these two clauses cannot be learned with that
ordering of examples.

This sensitivity of the TPC algorithm on the order of the examples can
be solved by learning in distinct _episodes_ where the clauses learned
in episode k are added to the background knowledge for episode k+1 and
then a new learning attempt is made. Episodic learning is not yet
implemented in Louise!


8. Discussion.

In all the examples above, we can note that the definition of the
invented predicate '$1' is a bit of a jumble. Ideally, we'd like to have
three separate invented predicates, one for each sub-program. That way
we wouldn't need to use metasubstitution constraints to shape the Top
Program- we could simply adjust the max_invented/1 option, to allow
learning more or fewer invented predicates.

Unfortunately this is not easy to do. The definition of the invented
predicate symbol '$1' is a the Top Program for that predicate given the
elements of the MIL problem. In other words, it's "all the ways to skin
the cat". This verbosity is characteristic of the Top Program and it is
also, counter-intuitively, at the root of its improved efficiency
compared to search-based procedures. It turns out it's faster to
directly construct all clauses in correct hypotheses than it is to
search the space of hypotheses for any one correct hypothesis, unless
the Hypothesis Space includes a very low number of hypotheses.

*/

%configuration:metarule_constraints(m(chain,_,'$1',_),fail).
%configuration:metarule_constraints(m(chain,'$1',a,s),fail).

:- auxiliaries:set_configuration_option(max_invented, [1]).

background_knowledge(s/2,[a/2,b/2]).

metarules(s/2,[chain]).

positive_example(s/2,E):-
	member(E, [s([a,a,b,b],[])
		  ,s([a,b],[])
%		  ,s([a,a,a,b,b,b],[])
%		  ,s([a,a,a,a,b,b,b,b],[])
%		  ,s([a,a,a,a,a,b,b,b,b,b],[])
%		  ,s([a,a,a,a,a,a,a,b,b,b,b,b,b,b],[])
		  ]).

negative_example(s/2,_):-
	fail.

a([a|T],T).
b([b|T],T).
