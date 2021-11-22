:-module(yamamoto, [background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ]).

:-use_module(configuration).

/** <module> Yamamoto's incompleteness result for Inverse Resolution.

In "Which Hypotheses Can be Found with Inverse Resolution" Akihiro
Yamamoto illustrates Inverse Resolution's incompleteness by
giving an example where the target theory does not subsume the bottom
clause. The example is as follows:

==
B2 = (even(0):- AND (even(s(x)):- odd(x))
E2 = odd(s(s(s(0)))):- .
==

Yamamoto proposes the following correct hypothesis:

==
H2 = odd(s(y)):- even(y).
==

Below we look at how Louise and Top Program Construction fare against
this example.

1. Listed queries work with the following configuration options:

==
?- list_config.
depth_limits(2,1)
example_clauses(call)
experiment_file(data/examples/yamamoto.pl,yamamoto)
generalise_learned_metarules(false)
learner(louise)
learning_predicate(learn_with_examples_invention/2)
max_invented(2)
metarule_formatting(quantified)
metarule_learning_limits(none)
minimal_program_size(2,inf)
prove_recursive(fast)
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

Ensure in particular that prove_recursive(fast) is set (otherwise,
learning with examples invention can enter an infinite recursion. This
is a bug).

2. The MIL problem for this experiment only gives a single example,
s^3(0), as in Yamamoto's paper:

==
?- list_mil_problem(odd/1).
Positive examples
-----------------
odd(s(s(s(0)))).

Negative examples
-----------------
[]

Background knowledge
--------------------
even/1:
even(0).
even(s(A)):-odd(A).

s/2:
s(s(A),A).

Metarules
---------
(Postcon-unit) ∃.P,Q,R ∀.x,y: P(x)← Q(x,y),R(y)
true.
==


3. Louise can invent new examples to learn the target theory:
==
?- examples_invention(odd/1).
odd(s(s(s(0)))).
odd(s(s(s(s(s(0)))))).
true.

?- learn_with_examples_invention(odd/1).
odd(A):-s(A,B),even(B).
true.
==


4. Without inventing new examples Louise's "vanilla" Top Program
Construction algorithm can't learn H2 from Yamamoto's setup, neither
can Louise's dynamic learning subsystem for predicate invention. The
hypotheses learned in both cases are over-specialisations:

==
?- learn(odd/1).
odd(s(s(s(0)))).
true.

?- learn_dynamic(odd/1).
odd(A):-s(A,B),s(B,C),s(C,D),even(D).
true.
==


3. Adding an example of the base-case, odd(s(0)), allows Louise to learn
the target theory:

==
?- list_encapsulated_problem(odd/1).
Positive examples
-----------------
m(odd,s(0)).
m(odd,s(s(s(0)))).

Negative examples
-----------------
[]

Background knowledge
--------------------
m(even,0).
m(even,s(A)):-p(odd,A).
m(s,s(A),A).
p(odd,s(0)).
p(odd,s(s(s(0)))).

Metarules
---------
m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E).
true.

?- learn(odd/1).
odd(A):-s(A,B),even(B).
true.

?- learn_dynamic(odd/1).
odd(A):-s(A,B),even(B).
true.
==


4. We can move the base-case around and Louise learns the same
hypothesis (but with some overs-pecialised clauses):

==
?- list_encapsulated_problem(odd/1).
Positive examples
-----------------
m(odd,s(s(s(0)))).
m(odd,s(s(s(s(s(0)))))).

Negative examples
-----------------
[]

Background knowledge
--------------------
m(even,0).
m(even,s(A)):-p(odd,A).
m(s,s(A),A).
p(odd,s(s(s(0)))).
p(odd,s(s(s(s(s(0)))))).

Metarules
---------
m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E).
true.

?- learn(odd/1).
odd(s(s(s(0)))).
odd(A):-s(A,B),even(B).
true.

?- learn_dynamic(odd/1).
odd(A):-s(A,B),s(B,C),s(C,D),even(D).
odd(A):-s(A,B),even(B).
true.
==


5. When examples are further apart, the hypothesis over-specialises
again (the more general clause isn't learned anymore):

==
?- list_encapsulated_problem(odd/1).
Positive examples
-----------------
m(odd,s(s(s(0)))).
m(odd,s(s(s(s(s(s(s(0)))))))).

Negative examples
-----------------
[]

Background knowledge
--------------------
m(even,0).
m(even,s(A)):-p(odd,A).
m(s,s(A),A).
p(odd,s(s(s(0)))).
p(odd,s(s(s(s(s(s(s(0)))))))).

Metarules
---------
m(postcon_unit,A,B,C):-m(A,D),m(B,D,E),m(C,E).
true.

?- learn(odd/1).
odd(s(s(s(0)))).
odd(s(s(s(s(s(s(s(0)))))))).
true.

?- learn_dynamic(odd/1).
odd(A):-s(A,B),s(B,C),s(C,D),even(D).
true.
==

6. In order for Louise's Top Program Construction (TPC) algorithm to
one-shot learn Yamamoto's example, the TPC algorithm must be allowed to
prove examples by resolution with the metarules in the MIL problem. To
allow this, ensure the following options are set in the configuration:
==
prove_recursive(examples).
prove_recursive(self).
==

And make sure that the following option is _not_ set:
==
%prove_recursive(fast).
==

Now try the learning query:
==
?- learn(odd/1).
odd(A):-s(A,B),even(B).
true.
==

This time, Louise learns Yamamot's target theory without
over-specialising.

__Explanation___

Why can Louise now learn Yamamoto's target theory? What's missing from
Inverse Resolution (and from the "vanilla" version of TPC that only
proves positive examples by resolution with other examples and the
background knowledge) is the ability to self-resolve a clause in the
learned hypothesis. In Yamamoto's example, the single clause in the
target theory must resolve with itself a few times to prove the single
example. Inverse Resolution and "vanilla" TPC will only add a clause to
the hypothesis if a proof of an example with that clause succeeds. If a
clause can only resolve with itself, the clause must be added to the
hypothesis before using it in a proof, otherwise it can't be used to
prove an example. Neither Inverse Resolution, nor vanilla TPC add
clauses to a hypothesis before proving them. Metagol (the original MIL
system) does, but at a great cost, because if a clause added to the
hypothesis fails to entail an example, the MIL meta-interpreter has to
backtrack and undo every step of the proof that derived that clause, a
process colloquially known as _Penelopising_ a proof (after Odysseu's
wife, Penelope, who weaved all day and undid her weave every night to
hold off her suitors). This is particularly wasteful if a clause turns
out to be deriveable only by multiple steps of recursion.

TPC avoids Penelopising a proof by proving examples by resolution with
_metarules_ rather than hypothesised clauses. If a metarule entails an
example by self-resolution, there exists at least one instance of the
metarule that also entails the example. The metarule meta-interpreter in
the "advanced" TPC algorithm can derive such instances. Since the
metarules are general statements about what can be proven, they don't
have to be "undone" by backtarcking when a proof fails. The metarule
meta-interpreter simply keeps the metasubstitutions of metarules in a
successful proof, as usual.

Setting the option prove_recursive(self) causes the TPC
algorithm to use is specialised metarule meta-interpreter to attempt to
prove examples by resolution with a metarule, and by resolving a
metarule with itself. prove_recursive(self) only allows a metarule to
resolve with itself, but not other metarules. The latter is not
necessary for Yamamoto's example, but can be enabled by setting the
option prove_recursive(others).

However, this "advanced" version of TPC is more expensive than the
vanilla one (although its time complexity remains polynomial) hence the
existence of options to control which is used.

*/

configuration: postcon_unit metarule 'P(x):- Q(x,y), R(y)'.

configuration:learning_predicate(learn_with_examples_invention/2).

:- auxiliaries:set_configuration_option(max_invented, [2]).

background_knowledge(odd/1, [even/1,s/2]).

metarules(odd/1,[postcon_unit]).

positive_example(odd/1,odd(N)):-
        odd(N).

negative_example(_,_):-
        fail.

s(s(X),X).

even(0).
even(s(X)):-
        odd(X).

%/* 1,2. Yamamoto's setup:
odd(s(s(s(0)))).
%*/

/* 3. Adding the base case:
odd(s(0)).
odd(s(s(s(0)))).
*/


/* 4. Giving a different base case:
odd(s(s(s(0)))).
odd(s(s(s(s(s(0)))))).
*/

/* 5. Examples are further apart:
odd(s(s(s(0)))).
odd(s(s(s(s(s(s(s(0)))))))).
*/
