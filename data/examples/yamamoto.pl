:-module(yamamoto, [background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ,even/1
                   ,s/2
                   ,odd/1
                   ]).

:-use_module(configuration).

/** <module> Yamamoto's incompleteness result for Inverse Entailment.

In "Which Hypotheses Can be Found with Inverse Entailment?" published in
the proceedings of the 1997 ICLP, Akihiro Yamamoto illustrates Inverse
Entailment's incompleteness by giving an example where the target theory
does not subsume the bottom clause. The example is as follows:

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

__1. Known good configuration__

Listed queries work with the following configuration options. Important
ones are marked with an asterisk, "*":

==
?- list_config.
* clause_limit(1)
example_clauses(call)
* experiment_file(data/examples/yamamoto.pl,yamamoto)
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
* reduction(plotkins)
* resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.

==


__2. MIL problem elements__

The MIL problem for this experiment only gives a single positive
example, s^3(0), as in Yamamoto's paper:

==
?- list_mil_problem(odd/1).
Positive examples
-----------------
odd(s(s(s(0)))).

Negative examples
-----------------

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

Note the definition of s/2 that is not given in Yamamoto's paper. This
predicate is needed because the hypotheses learned by Louise are
datalog, meaning that they have no function terms as arguments (for
example, s(0) is a functional term).

The practice of replacing a function term with a new predicate
definition is called _flattening_ and is described in "Flattening and
Saturation: Two Representation Changes for Generalisation" by Celine
Rouveirol published in the journal Machine Learning, issue 14, 1994.


__3. Learning query__

Make a learning attempt. The results should look like this:

==
?- list_learning_results.
odd(A):-s(A,B),even(B).
==

Not the flattening of s(N) to s(A,B).


__4. Overgeneralising__

Above, we have set the configuration option clause_limit/1 to "1". This
means that Louise will only learn a single clause at a time, but allow
the clause to resolve any number of times with itself. This is
sufficient to learn Yamamoto's definition of odd/1. It is also a way to
avoid an over-general hypothesis.

Try setting the clause limit to "2", by modifying the following
directive (already declared in the source of the experiment file,
below):

==
:- auxiliaries:set_configuration_option(clause_limit, [2]).
==

Remember to call make/0 to actually modify the setting. Check that the
setting is modified with list_config/0:

==
?- list_config.
clause_limit(2)
% ... more options
==

Now make a new learning attempt:

==
?- learn(odd/1).
odd(A):-s(A,B),even(B).
odd(A):-s(A,B),odd(B).
true.
==

This time, an over-general hypothesis is learned. This over-general
hypothesis can be specialised with a suitable negative example. That is
left as an exercise to the reader (yes, you).

Note that the two clauses above are the only clauses that can be learned
from the elements of Yamamoto's problem, regardless of the clause limit.
For example, try setting the clause limit to 20:

==
:- auxiliaries:set_configuration_option(clause_limit, [20]).
==

And then repeat the learning query above.


__5. Conclusions__

Louise's Top Program Construction algorithm (TPC) can learn odd/1 with
the elements in Yamamoto's problem, modulo flattening.

What's missing from Inverse Entailment is the ability to resolve a
clause with other clauses in a hypothesis being learned, including the
clause itself. In Yamamoto's example, the single clause in the target
theory must resolve with itself a few times to prove the single example.
Inverse Entailment will only add a clause to a hypothesis if a proof
of an example with that clause succeeds. If a clause can only resolve
with itself, the clause must be added to the hypothesis before using it
in a proof, otherwise it can't be used to prove an example. Inverse
Entailment will not add clauses to a hypothesis before proving them and
so cannot learn such hypotheses.

Metagol, the original Meta Interpretive Learning (MIL) system does do
that, but at a great cost. If a clause added to the hypothesis does not
entail any positive examples, or entails a negative example, the MIL
meta-interpreter has to backtrack and undo every step of the proof that
derived that clause, a process colloquially known as _Penelopising_ a
proof (after Odysseu's wife, Penelope, who weaved all day and undid her
weave every night to hold off her suitors until her husband could come
home and massacre them). This is particularly wasteful if a clause turns
out to be deriveable only by multiple steps of recursion, and then is
found to be over-general, or over-special.

TPC avoids Penelopising a proof by proving positive examples one-by-one,
and then "stitching together" the sets of clauses learned from each
example. Then, TPC attempts to prove each _negative_ example with a
learned clause and throws out each clause that entails a negative
example.

The downside is that TPC will derive _all_ clauses that entail _each_
positive example. That may include over-general clauses that can only
be gotten ride off by negative examples. If negative examples are not
available, the learned hypothesis will be over-general.

(In truth, if we don't have negative examples it's not strictly correct
to speak of "over-general" hypotheses: a hypothesis is over-general if
it covers negative examples. But in this case we use "over-general" to
mean a hypothesis that does not match the target theory, which we
alerady know.)

*/

configuration: postcon_unit metarule 'P(x):- Q(x,y), R(y)'.

:- auxiliaries:set_configuration_option(clause_limit, [1]).

background_knowledge(odd/1, [even/1,s/2]).

metarules(odd/1,[postcon_unit]).

positive_example(odd/1,odd(N)):-
        odd(N).

negative_example(odd/1,_):- fail.

s(s(X),X).

even(0).
even(s(X)):-
        odd(X).

odd(s(s(s(0)))).
