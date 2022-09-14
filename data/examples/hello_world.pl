:-module(hello_world, [background_knowledge/2
                      ,metarules/2
                      ,positive_example/2
                      ,negative_example/2
                      ,ancestor/2
                      ,parent/2
                      ]).

/** <module> A simple introduction to learning logic programs with Louise

__Introduction__

The examples in this experiment file are an introductory, "hello-world"
type of experiment to help the new user understand how to begin to use
Louise. The examples below cover the basics of learning a simple
recursive program.


__Starting configuration__

The experiments below were run with the following configuration
options. Important options are highlighted with an asterisk ("*"):

==
?- list_config.
* clause_limit(1)
depth_limits(2,1)
example_clauses(call)
* experiment_file(data/examples/hello_world.pl,hello_world)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
* max_invented(0)
metarule_formatting(quantified)
metarule_learning_limits(metasubstitutions(1))
minimal_program_size(2,inf)
recursive_reduction(false)
reduce_learned_metarules(false)
* reduction(plotkins)
resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.
==

The configuration options above will change during the experiment but if
at some point you find that the output you get is different than the one
listed in this example, make sure your configuration matches the listing
above and take it from there.


__Editing the configuration__

Configuration options can be set by editing Louise's main configuration
file, configuration.pl, found at the project's root directory.

Alternatively configuration options can be edited by calling the
predicate set_configuration_option/2. This predicate can be declared as
a directive in the experiment file. For example, this experiment file
declares the following directives setting configuration options:

==
:- auxiliaries:set_configuration_option(clause_limit, [1]).
:- auxiliaries:set_configuration_option(reduction, [plotkins]).
==

The first argument of set_configuration_option/2 is the name of the
option and the second argument is its value. When the experiment file is
saved and reloaded the set_configuration_option/2 directives are called
and modify the values of configuration options in memory.


__Reloading an experiment file__

After making changes to the experiment file you can reload it by calling
SWI-Prolog's make/0 predicate. You should see output like the following:

==
?- make.
% c:/.../louise/data/examples/hello_world compiled into hello_world 0.00 sec, 0 clauses
true.
==


__Experiment files__

Louise's training data is declared in a Prolog module file with a
specific structure. We call this file an "experiment file". The file
you are looking at right now is an experiment file!

Only one experiment file can be loaded in memory at a time.

The current experiment file is defined in the configuration option
experiment_file/2. For example, this experiment file is loaded into
memory by declaring the following in configuration.pl:

==
experiment_file('data/examples/hello_world.pl',hello_world).
==

Experiment files must export four predicates storing the examples,
background knowledge and metarules for a learning problem. You will see
that the current experiment file has this module header at the top:

==
:-module(hello_world, [background_knowledge/2
                      ,metarules/2
                      ,positive_example/2
                      ,negative_example/2
                      ,ancestor/2
                      ,parent/2
                      ]).
==

The first four exported predicates, background_knowledge/2, metarules/2,
positive_example/2 and negative_example/2 are the "experiment file
interface predicates" that hold the training data.

The predicates ancestor/2 and parent/2 are given as background knowledge
and are exported to make it easier for Louise to locate their
definitions in Prolog's memory.


__Listing the elements of a learning problem__

The experiment file interface predicates are not meant to be called
directly. Instead, Louise provides an auxiliary predicate (i.e. one
defined in its src/auxiliaries.pl module) called list_mil_problem/1,
that can be used to list the elements of a learning problem in a
user-friendly format.

Below, we call list_mil_problem/1 to list the elements of the learning
problem for ancestor/2, as defined in this experiment file:

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

For the ancestor/2 example we have a few positive and negative examples,
an extensional definition of parent/2 as background knowledge and two
metarules, "Tailrec" and "Identity".

Louise can use intensionally defined predicates as background knowledge
just as well as extensionally defined ones. For example, we could define
parent/2 as follows:

==
parent(X,Y):- father(X,Y).
parent(X,Y):- mother(X,Y).
==

But just for simplicity in this example we only use extensional
definitions.

The metarules are second-order clauses that entail the positive examples
with respect to the background knowledge. When a hypothesis is learned
from the elements of the MIL problem its first-order clauses are
instances of the metarules with existentially-quantified variables
replaced by predicate symbols. Those predicate symbols are taken from
the examples and the background knowledge, and any invented predicates.


__A first learning query__

Now that we know the elements of our learning problem we can make a
first learning attempt. This is done by calling Louise's main learning
predicate, learn/1. Here's what this looks like:

==
?- learn(ancestor/2).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
true.
==

The single argument of learn/2 is the name of a learning target defined
in the current experiment file (same as list_mil_problem/1). learn/1
tries to learn all it can from the learning problem and prints the
learned hypothesis at the Prolog console (the "top-level").

The output of learn/1 above is a (left-) recursive definition of
ancestor/2.

The base-case, "ancestor(A,B):-parent(A,B)", is an instance
of the Identity metarule: P(X,Y):- Q(X,Y). To form the base-case clause,
Louise replaced the second-order variables P and Q in Identity with the
predicate symbols "ancestor" and "parent", respectively. Those predicate
symbols are taken from the background knowledge and the positive
examples.

The inductive case, "ancestor(A,B):-ancestor(A,C),ancestor(C,B)" is an
instance of the Tailrec metarule: P(X,Y):- Q(X,Z), P(Z,Y) with P and Q
both replaced by "ancestor".

Tailrec is an obligatory recursive metarule, meaning that its first and
last literal have the same second-order variable. Nothing forces it to
bind the second order variable in the second literal to "ancestor" also.
In fact, as we will see, Louise has derived more instances of the
metarules than the ones output by learn/1, above.


__Program reduction__

The program learned by Louise is the Top Program, the set of clauses in
_all_ correct hypotheses that can be derived from a learning problem. A
"correct hypothesis" is a logic program that entails all the positive
examples, and no negative example, with respect to background knowledge.

Often the Top Program will include redundant clauses, that are entailed
by other clauses in the Top Program. Louise removes those clauses by
passing the Top Program through its implementation of Gordon Plotkin's
program reduction algorithm.

We can see the effect of program reduction if we turn it off, which we
can do in the configuration. To do this now, uncomment the following
clause in the experiment file, after this comment section:

==
:- auxiliaries:set_configuration_option(reduction, [none]).
==

Remember to call make/0 to reload the experiment file and execute the
changed directive.

Now make another learning attempt:

==
?- learn(ancestor/2).
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true.
==

Note the two new clauses that appeared in the output:

==
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
==

The first clause is an instance of Identity. It is also a tautology,
which means it is always redundant, so Plotkin's algorithm removes it.

The second clause is not a tautology, but it is a special case of the
inductive-case clause we saw in the previous learning attempt:

==
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
==

ancestor(A,B):-ancestor(A,C),ancestor(C,B) entails
ancestor(A,B):-parent(A,C),ancestor(C,B) with respect to background
knowledge. Entailment denotes generalisation, so Plotkin's algorithm
removes the more special clause.


__Clause limits__

Recall that we have started the experiment with the following
configuration option:

==
?- list_config.
* clause_limit(1)
% .... more options
==

The option clause_limit/1 limits the number of instances of metarules
that can be instantiated during the proof of a single example, by
Louise's inductive meta-interpreter. Meta-Interpretive Learning is
called that beause it uses a modified Prolog meta-interpreter to learn!

With clause_limit(1) Louise can only derive a single instance of a
single metarule during a proof. But in the previous sections we saw
Louise learn a hypothesis with more than one clause, and with recursion!
Why is that?

Louise learns by constructing all clauses that entail each of its
examples in turn. It just so happens that the learning problem for
ancestor/2 includes enough examples to learn all four clauses listed in
the previous section (Section 9):

==
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
==

In other words, each of the four clauses above was learned separately
and in isolation: either by a different example, or by the same example
on backtracking.

As we have seen, those four clauses include two recursive ones. In order
for a recursive clause to entail an example it must have a base-case (or
recursion will either fail, or "go infinite"). However, with a clause
limit of 1, Louise can only learn one clause at a time, so it can't
possibly learn both a recursive clause and a base case. That's two
clauses!

What happens then? When clause_limit(1) is set, Louise adds the positive
examples to the background knowledge. Those positive examples then act
as a "base case". That way Louise can learn recursion.

But note that this is only a limited form of recursion! In particular,
only clauses that can resolve with the positive examples can be learned
this way.

Note well: a recursive clause learned by resolution with the positive
examples can also resolve with other clauses, or itself, which is the
case with the two recursive clauses of ancestor/2. But such clauses can
only be _derived_ by resolution with positive examples, as long as
clause_limit(1) is set.

In ILP, learning a single clause at a time is sometimes known as
"single-claue learning".


__Single-clause learning__

Learning one clause at a time can be limiting. To see how, comment-out
the following clause of positive_example/:

==
% positive_example(ancestor/2,ancestor(A,B)):- ancestor(A,B).
==

And then uncomment this one:

==
positive_example(ancestor/2,ancestor(stathis,stassa)).
==

This is what our learning problem looks like now:

==
?- list_mil_problem(ancestor/2).
Positive examples
-----------------
ancestor(stathis,stassa).

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

We have a single positive example and everything else remains unchanged.

Remember to call make/0 to load the experiment file, and its changes, to
memory.

Now make another learning attempt:

==
?- learn(ancestor/2).
ancestor(A,B):-ancestor(A,B).
true.
==

This time, we couldn't learn much! Only the tautology that tells us a
person's ancestor is... their ancestor.

If you look at the definitions of ancestor/2 and parent/2 in our
background knowledge, you will see that "stathis" is declared as the
ancestor of "stassa", but there is no clause of parent/2 that connects
"stathis" to "stassa".

==
% This is true:
ancestor(stathis,stassa).

% This is not true:
parent(stathis,stassa).
==

In other words, "stathis" is the ancestor of "stassa" but not the
parent. So there is no example from which to learn the instance of
Identity that tells us that a parent is also an ancestor:

==
ancestor(A,B):-parent(A,B)
==

We can't learn these two recursive clauses either:

==
ancestor(A,B):-parent(A,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
==

And that is because in order to prove those we have to "walk over" the
transitive closure of the ancestor/2 relation - but we cannot do that in
a single clause, and that is the restriction imposed by clause_limit(1).


__Multi-clause learning__

To learn more than one clause at a time from an example, we must
increase the clause limit to something more than 1.

Do this now, by changing the value of clause_limit/1 in the
set_configuration_option/2 directive:

==
:- auxiliaries:set_configuration_option(clause_limit, [2]).
==

Remember to reload the experiment file by calling make/0. You can check
the configuration again to make sure the value of clause_limit/1 is
right:

==
?- list_config.
clause_limit(2)
% ... more options
==

Now make another learning attempt:

==
?- learn(ancestor/2).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true.
==

This time, Louise learns both recursive clauses and their common
base-case.

Note that that tautological clause, ancestor(A,B):-ancestor(A,B), that
was learned earlier is absent this time. This is because with
clause_limit(2), Louise does not add the examples to the background
knowledge anymore and the tautological clause cannot be derived without
them.

Louise can still learn the tautology if we increase the clause limit
further.

To see this, set the value of clause_limit/1 to 3:

==
:- auxiliaries:set_configuration_option(clause_limit, [3]).
==

Remember to reload the experiment file in memory by calling make/0 and
check that the configuration has changed as we've seen before.

Now make another learning attempt:

==
?- learn(ancestor/2).
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true.
==

With a clause limit of 3 the resolution proof has enough depth to reach
the "ancestor(A,B):-ancestor(A,B)" clause. In other words, the clause
limit is also the depth of the resolution proof.

What happens if we increase the clause limit further? Let's try it. Set
clause_limit/1 to 4 and make another learning attempt:

==
?- list_config.
clause_limit(4)
% ... more options

?- learn(ancestor/2).
ancestor(A,B):-ancestor(A,B).
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).
ancestor(A,B):-parent(A,C),ancestor(C,B).
true.
==

There are no more clauses to learn from our single example no matter how
deep the proof goes. From this point on, increasing the clause limit
will only make learning cost more, without producing better results.

Unfortunately, there is no principled way to choose the appropriate
clause limit for a learning attempt. Finding the right clause limit is
something that must be done by trial and error (and usually, mostly
error).


__More examples__

In this simple example we have seen Louise to learn a simple program
with recursion and how to control its resolution-proof depth by setting
the configuration option clause_limit/1.

Louise is capable of more advanced learning, in particular, predicate
invention, which can extend the background knowledge given by the user;
and learning new metarules. These more advanced subjects are
discussed in other experiment files in data/examples:

anbn.pl (predicate invention with recursion).
recipes.pl (predicate invention withour recursion).
tiny_kinship_meta.pl (learning metarules).


__Configuration errors__

Here's a bonus tip that can make your life easier when working with
Louise.

Editing the configuration file and the currently loaded experiment file
(this file!) one after the other causes SWI-Prolog to raise an annoying
error after which you have to start a new SWI-Prolog session.

For example, after editing configuration.pl and this experiment file,
and calling make/0, you might see error text like the following:

==
?- make.
ERROR: c:/.../louise/data/examples/hello_world.pl:222:
ERROR:    No permission to redefine imported_procedure `hello_world:background_knowledge/2'
ERROR: c:/.../louise/data/examples/hello_world.pl:224:
ERROR:    No permission to redefine imported_procedure `hello_world:metarules/2'
ERROR: c:/.../louise/data/examples/hello_world.pl:226:
ERROR:    No permission to redefine imported_procedure `hello_world:positive_example/2'
ERROR: c:/.../louise/data/examples/hello_world.pl:231:
ERROR:    No permission to redefine imported_procedure `hello_world:negative_example/2'
ERROR: c:/.../louise/data/examples/hello_world.pl:234:
ERROR:    No permission to redefine imported_procedure `hello_world:parent/2'
==

To avoid this kind of error, prefer to use set_configuration_option/2
directives to change configuration options, rather than editing the
configuration directly.

*/

:- auxiliaries:set_configuration_option(clause_limit, [1]).
:- auxiliaries:set_configuration_option(reduction, [plotkins]).

%!	background_knowledge(+Target,-Symbols) is semidet.
%
%	Background knowledge Symbols for a learning Target.
%
background_knowledge(ancestor/2,[parent/2]).

%!	metarules(+Target, -Metarules) is semidet.
%
%	IDs of teh Metarules for a learning Target.
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
