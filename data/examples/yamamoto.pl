:-module(yamamoto, [background_knowledge/2
                   ,metarules/2
                   ,positive_example/2
                   ,negative_example/2
                   ,even/1
                   ,s/2
                   ,odd/1
                   ]).

:-use_module(configuration).

/** <module> Yamamoto's incompleteness result for Inverse Resolution.

In "Which Hypotheses Can be Found with Inverse Resolution?" Akihiro
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

__1. Known good configuration__

Listed queries work with the following configuration options. Important
ones are marked with an asterisk, "*":

==
?- list_config.
* clause_limit(2)
depth_limits(2,1)
example_clauses(call)
* experiment_file(data/examples/yamamoto.pl,yamamoto)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
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

The MIL problem for this experiment only gives a single example, s^3(0),
as in Yamamoto's paper:

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

__3. Learning query__

Make a first learning attempt. It should look like this:

==
?- list_learning_results.
odd(A):-s(A,B),even(B).
odd(A):-s(A,B),odd(B).

true.
==

Clearly, that's over-general. But it is a correct hypothesis! Both
clauses entail our single example, of s^3(0).

To avoid over-generalisation, we need to add a negative example. s^2(0)
will do. It is already defined in the source code of this experiment
file, after this comment section, but it is commented out:

==
negative_example(odd/1,E):-
        member(E, [%odd(s(s(0)))
                  ]).
==

Comment it back in and reload the experiment file with make/0.

The elements of the MIL problem for odd/1 should now look as follows:

==
?- list_mil_problem(odd/1).
Positive examples
-----------------
odd(s(s(s(0)))).

Negative examples
-----------------
:-odd(s(s(0))).

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

Now make a new learning attempt. It should look like this:

==
?- list_learning_results.
odd(A):-s(A,B),even(B).

true.
==

This time there is no over-generalisation. The negative example excludes
the over-general clause learned earlier.


__4. Conclusions__

Louise's Top Program Construction algorithm (TPC) can learn odd/1 with
the elements in Yamamoto's problem.

What's missing from Inverse Resolution is the ability to resolve a
clause with other clauses in the learned hypothesis, including itself.
In Yamamoto's example, the single clause in the target theory must
resolve with itself a few times to prove the single example. Inverse
Resolution will only add a clause to the hypothesis if a proof of an
example with that clause succeeds. If a clause can only resolve with
itself, the clause must be added to the hypothesis before using it in a
proof, otherwise it can't be used to prove an example. Inverse
Resolution, will not add clauses to a hypothesis before proving them.

Metagol, the original Meta Interpretive Learning (MIL) system does do
that, but at a great cost. If a clause added to the hypothesis does not
entail any positive examples, or entails a negative example, the MIL
meta-interpreter has to backtrack and undo every step of the proof that
derived that clause, a process colloquially known as _Penelopising_ a
proof (after Odysseu's wife, Penelope, who weaved all day and undid her
weave every night to hold off her suitors). This is particularly
wasteful if a clause turns out to be deriveable only by multiple steps
of recursion, and then is found to be over-general, or over-special.

TPC avoids Penelopising a proof by proving examples by resolution with
_metarules_ rather than hypothesised clauses. If a metarule entails an
example by resolution with the background knowledge or metarules
(including itself), there exists at least one instance of the metarule
that also entails the example. The metarule meta-interpreter in the
implementation of TPC in Louise can derive such instances. Since the
metarules are general statements about what can be proven, they don't
have to be "undone" by backtarcking when a proof fails. The metarule
meta-interpreter simply keeps the metasubstitutions of metarules in a
successful proof, as usual.

The downside is that TPC will derive _all_ clauses that entail a
positive example. As seen in the example above, that may include
over-general clauses. In that case, negative examples are needed.

In truth, if we don't have negative examples it's not strictly correct
to speak of "over-general" hypotheses: a hypothesis is over-general if
it coverse negative examples. But in this case we need a hypothesis that
does not match the target theory, which we alerady know.

*/

configuration: postcon_unit metarule 'P(x):- Q(x,y), R(y)'.

:- auxiliaries:set_configuration_option(clause_limit, [2]).

background_knowledge(odd/1, [even/1,s/2]).

metarules(odd/1,[postcon_unit]).

positive_example(odd/1,odd(N)):-
        odd(N).

% Uncomment example to avoid learning over-general clauses
negative_example(odd/1,E):-
        member(E, [%odd(s(s(0)))
                  ]).

s(s(X),X).

even(0).
even(s(X)):-
        odd(X).

odd(s(s(s(0)))).
