:-module(hello_world, [background_knowledge/2
                      ,metarules/2
                      ,positive_example/2
                      ,negative_example/2
                      ,ancestor/2
                      ,parent/2
                      ]).

:-use_module(configuration).


/** <module> A simple introduction to learning logic programs with Louise

__Check starting configuration__

To begin the experiment, make sure that the configuration options in
configuration.pl match the ones listed below. Important options are
marked with an asterisk "*".

==
?- list_config.
* clause_limit(0)
example_clauses(call)
* experiment_file(data/examples/hello_world.pl,hello_world)
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

__List the elements of the learning problem__

Next, check that the elements of thelearning problem declared in this
experiment file match the listing below.

==
?- list_mil_problem(ancestor/2).
Positive examples
-----------------
ancestor(stathis,kostas).
ancestor(stefanos,dora).
ancestor(kostas,stassa).
ancestor(alexandra,kostas).
ancestor(paraskevi,dora).
ancestor(dora,stassa).
ancestor(stathis,stassa).
ancestor(stefanos,stassa).
ancestor(alexandra,stassa).
ancestor(paraskevi,stassa).

Negative examples
-----------------
:-ancestor(kostas,stathis).
:-ancestor(dora,stefanos).
:-ancestor(stassa,kostas).
:-ancestor(kostas,alexandra).
:-ancestor(dora,paraskevi).
:-ancestor(stassa,dora).
:-ancestor(stassa,stathis).
:-ancestor(stassa,stefanos).
:-ancestor(stassa,alexandra).
:-ancestor(stassa,paraskevi).

Background knowledge
--------------------
parent/2:
parent(stathis,kostas).
parent(stefanos,dora).
parent(kostas,stassa).
parent(alexandra,kostas).
parent(paraskevi,dora).
parent(dora,stassa).

Metarules
---------
(Tailrec) ∃.P,Q ∀.x,y,z: P(x,y)← Q(x,z),P(z,y)
(Identity) ∃.P,Q ∀.x,y: P(x,y)← Q(x,y)

true.
==


__Make a learning query__

Finally, call Louise's learn/1 predicate to learn a definition of the
"target predicate" in the positive examples, i.e. ancestor/2.

==
?- learn(ancestor/2).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true.
==

In ILP, we refer to the program learned by a learning system as a
"hypothesis". The output of learn/1, above, is the hypothesis learned by
Louise from the training data listed earlier.

Note that Louise learns a recursive hypothesis. The configuration option
clause_limit(0) is sometimes enough to learn a recursive hypothesis, but
this depends on the training data. In this case, there are enough
examples of ancestor/2 to learn a recursive hypothesis of it, but for
many other learning problems the clause limit must be set to something
higher. See the example files anbn.pl, yamamoto.pl, ackermann.pl and
even_odd.pl for more on learning recursion with Louise.

*/

%!	background_knowledge(+Target,-Symbols) is semidet.
%
%	Background knowledge Symbols for a learning Target.
%
%	Background knowledge is a set of definite program definitions
%	used to compose a new hypothesis.
%
background_knowledge(ancestor/2,[parent/2]).

%!	metarules(+Target, -Metarules) is semidet.
%
%	IDs of the Metarules for a learning Target.
%
%	Metarules are defined in the configuration file. They can also
%	be defined in experiment files. See
%	data/examples/user_metarules.pl for an example of defining your
%	own metarules.
%
metarules(ancestor/2,[tailrec,identity]).

%!	positive_example(+Target,-Examples) is nondet.
%
%	Generator of positive Examples for a learning Target.
%
positive_example(ancestor/2,ancestor(A,B)):-
	ancestor(A,B).
%positive_example(ancestor/2,ancestor(stathis,stassa)).

%!	negative_example(+Target,-Examples) is nondet.
%
%	Generator of negative Examples for a learning Target.
%
negative_example(ancestor/2,ancestor(A,B)):-
	ancestor(B,A).

parent(stathis,kostas).
parent(stefanos,dora).
parent(kostas,stassa).
parent(alexandra,kostas).
parent(paraskevi,dora).
parent(dora,stassa).

ancestor(stathis,kostas).
ancestor(stefanos,dora).
ancestor(kostas,stassa).
ancestor(alexandra,kostas).
ancestor(paraskevi,dora).
ancestor(dora,stassa).
ancestor(stathis,stassa).
ancestor(stefanos,stassa).
ancestor(alexandra,stassa).
ancestor(paraskevi,stassa).
