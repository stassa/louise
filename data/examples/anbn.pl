:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,a/2
	       ,b/2
	       ]).

/** <module> Learn an a^nb^n CFG with recursion and predicate invention.

__Table of contents__

Use the following section headers to navigate to each section in this
instructions document.

==
1. Introduction
2. Known good configuration.
3. MIL Problem elements.
4. Make a first learning attempt.
5. Eyballing results.
6. Unfolding to remove invented predicates.
7. Weakenig reduction.
8. Use of constraints.
9. Discussion.
==

"Use" means copy each section header above and CTRL-F to find it in the
text. No hyperlinks here, sorry!


__1. Introduction__

The example in this experiment file llustrates the use of predicate
invention to learn a recursive theory; the use of unfolding to remove
invented predicates from a learned hypothesis; and the use of
metasubstitution constraints to avoid adding unwanted clauses to the
learned hypothesis.

a^nb^n is a context-free language that includes only strings of the
character a repeated n times, followed by the character b repeated n
times.

The following is a grammar of a^nb^n in Chomsky Normal Form:

==
s --> ab
s --> asb
==

In Prolog, we can define the same grammar of a^nb^n using Definite
Clause Grammars notation (DCG), as follows:

==
s --> a, b.
s --> a, s, b.
==

Expanded into ordinary Prolog definite clauses, that's the same as:

==
s(X,Y):- a(X,Z), b(Z,Y).
s(X,Y):- a(X,Z), s(Z,U), b(U,Y).
==

The above set of definite clauses will be our target theory, i.e. the
program we will try to learn from examples and background knowledge. We
will start with a single positive example and a few negative examples,
and our background knowledge will be the DCG definitions of the
pre-terminals a and b.


__2. Known good configuration.__

The results listed below were obtained with the following configuration
options defined in configuration.pl. Required options are marked with
"*":

==
?- list_config.
* clause_limit(3)
example_clauses(call)
* experiment_file(data/examples/anbn.pl,anbn)
* fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
listing_limit(10)
max_error(0,0)
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

If the output you see looks different adjust the configuration to match
the listing above.

In particular, make sure that the following options are set:
==
clause_limit(3)
max_invented(1)
==

The option clause_limit(3) tells Louise to try and learn up to 3 clauses
from each positive example. This is necessary because the (known) target
theory for this experiment has 3 clauses.

The option max_invented(1) tells Louise to attempt to define at least
one invented predicate.

Both these options can be set in the configuration, or by calling the
auxiliary predicae set_configuration_option/2 as a directive from this
experiment file. set_configuration_option/2 modifies configuration
options declared dynamic and already loaded in memory.

The following directives are declared by default in this experiment
file:

==
:- auxiliaries:set_configuration_option(clause_limit, [3]).
:- auxiliaries:set_configuration_option(max_invented, [1]).
==

You can play with these configuration options without having to modify
the configuration file by modifying these two directives. Modifying the
configuration file and an experiment file in succession leads to ugly
errors.


__3. MIL Problem elements.__

Ensure also that the output of list_mil_problem/1 matches the following,
to make sure the elements of the MIL problem are as expected by the
instructions in this file:

==
?- list_mil_problem(s/2).
Positive examples
-----------------
s([a,a,b,b],[]).

Negative examples
-----------------
:-s([a,a],[]).
:-s([b,b],[]).
:-s([a,a,b],[]).
:-s([a,b,b],[]).

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


__4. Make a first learning attempt.__

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
neither are training examples of it provided by the user. '$1'/2 is
learned in the process of learning the target predicate s/2.


__5. Eyballing results.__

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
Sub-program 3 is an over-specialisation that only covers our single
example, s([a,a,b,b],[]) (i.e. the string aabb).


__6. Unfolding to remove invented predicates.__

Louise can unfold a learned hypothesis to remove invented symbols.

Set the configuration option unfold_invented/1 to "true" to unfold
the program learned in the previous step. You can do that with
set_configuration_option/2, already declared in this experiment file.
Change its value to "true":

==
:- auxiliaries:set_configuration_option(unfold_invented, [true]).
==

Now make another learnign attempt:

==
?- learn(s/2).
s(A,B):-a(A,C),a(C,D),b(D,E),b(E,B).
s(A,B):-a(A,C),s(C,D),b(D,B).
true.
==

Note that the unfolded hypothesis is _not_ equivalent to the Top Program
in the previous section. In particular, the unfolded program is missing
the base-case of the recursive hypothesis:

==
% Base-case:
s(A,B):-a(A,C),b(C,B).
==

The unfolded hypothesis is broken because Louise's unfolding code only
returns clauses that, when unfoled, entail a positive example. To learn
a hypothesis that unfolds to the target theory, we must add an example
of the base-case to the positive examples (it is not currently possible
to give examples only to the unfolding operation).

An example of the base-case is included in positive_example/2, below,
but it is commented-out. Comment it back in to continue with the
experiment.

Remember to rebuild the project with a call to make/0, and list the
elements of the learning problem again to make sure they look like this:

==
?- list_mil_problem(s/2).
Positive examples
-----------------
s([a,b],[]).
s([a,a,b,b],[]).

Negative examples
-----------------
:-s([a,a],[]).
:-s([b,b],[]).
:-s([a,a,b],[]).
:-s([a,b,b],[]).

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

Now make another learning attempt:

==
?- learn(s/2).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,D),b(D,B).
true.
==

Unfolding removes clauses of invented predicates from the learned
program and replaces them with equivalent clauses.

Louise's unfolding code replaces clauses with invented predicates that
resolve with each other, with each of their resolvents. Only resolvents
that entail a positive example are kept in the finally output
hypothesis.

In the output above, the first clause in the unfolded program is the
base-case, covering the example we added of an anbn string where n=1
(i.e. the base-case of the recursive hypothesis). This clause is not
affected by unfolding because it has no literals with invented predicate
symbols.

The recursive clause is a three-literal clause that is equivalent to the
first and second clause in both Sub-program 1 and Sub-program 2. The
difference is only in the order in which the invented literal appears in
the body of these two sub-programs. There are many ways to skin that
cat! Top Program Construction by default learns them all.

Note that in the unfolded program above, there is no clause equivalent
to the clauses in Sub-program 3 (other than the base-case). This is
because, by default, Louise applies program reduction to an unfolded
program. Reduction of the unfolded program is controlled by the setting
of the configuration option reduction/1. This is the same configuration
option that constrols the reduction of the Top Program right after
learning (and before unfolding). There is no option to reduce the Top
Program only before or only after unfolding!


__7. Weakenig reduction.__

To inhibit reduction and reveal the unfolded clause that is
equivalent to Sub-program 3 in the learned Top Program, set the
configuration option reduction/1 to "none". Again, you can do this with
a call to set_configuration_option/2:

==
:- auxiliaries:set_configuration_option(reduction, [none]).
==

Remember to make/0 the project. You can also call list_config/0 to make
sure the value of the configuration option loaded in memory has changed:

==
?- list_config.
clause_limit(3)
% ... more options
 reduction(none) % <-- this should be "none"
% ... more options
unfold_invented(true)
true.
==

Now make another learning attempt:

==
?- learn(s/2).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),a(C,D),b(D,E),b(E,B).
s(A,B):-a(A,C),s(C,D),b(D,B).
true.
==

The second clause in the unfolded, but unreduced, Top Program printed
above, s(A,B):-a(A,C),a(C,D),b(D,E),b(E,B), is an over-specialisation.
That is in the sense that it only covers the n=2 example (the string
aabb, i.e. our training example s([a,a,b,b],[])). That clause is also
redundant given the other two clauses since those two clauses together
suffice to cover the n=2 example. Hence that clause is removed by
program reduction.


__8. Use of constraints.__

Metarule constraints can be used to shape the Top Program to your needs,
for example to remove unwanted clauses from the learned program. This is
most useful when you have no negative examples and observe a clear
over-generalisation caused by spurious recursion.

To begin with, comment-out all the negative examples of s/2 defined in
negative_example/2. Call make/0 to rebuild the project and check that
there are now no negative examples loaded:

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

Note that we have kept the base-case example.

Ensure also that the following options are set:

==
:- auxiliaries:set_configuration_option(unfold_invented, [false]).
:- auxiliaries:set_configuration_option(reduction, [none]).
==

That's in order to unfold the learned hypothesis and marvel at it in
all it sglory.

Make a new learning attempt with the query listed below:

==
?- learn(s/2, _Ps), print_clauses(_Ps), length(_Ps,N).
'$1'(A,B):-'$1'(A,C),b(C,B).
'$1'(A,B):-a(A,C),'$1'(C,B).
'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-a(A,C),b(C,B).
'$1'(A,B):-a(A,C),s(C,B).
'$1'(A,B):-b(A,C),b(C,B).
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-'$1'(A,C),'$1'(C,B).
s(A,B):-'$1'(A,C),b(C,B).
s(A,B):-'$1'(A,C),s(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),a(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-b(A,C),b(C,B).
s(A,B):-s(A,C),'$1'(C,B).
s(A,B):-s(A,C),b(C,B).
s(A,B):-s(A,C),s(C,B).
N = 18.
==

Above, we have used the learn/2 variant to output the learned hypothesis
as a list of clauses. That's just to be able to measure its cardinality
with length/2. learn/2 doesn't print the learned hypothesis to the
top-level, as learn/1 does, so we also call print_clauses/1 to do the
job.

Clearly, the program listed in the output above is over-general. It's
not just that it has too many clauses. If you load that program in
memory, you'll see that in tentails strings that do not belong to the
anbn language.

For example:

==
?- anbn:s([a,b,b,b],B).
B = [] ;
B = [b] ;
B = [b, b].
==

To produce the output above, the hypothesis learned in the previous
learning attempt was added to this experiment file and the file
reconsulted. If you want to try it yourself, note that in order to
avoid infinite left-recursions, s/2 and '$1'/2 need to be tabled:

==
:-table(s/2).
:-table('$1'/2).

'$1'(A,B):-'$1'(A,C),b(C,B).
'$1'(A,B):-a(A,C),'$1'(C,B).
'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-a(A,C),b(C,B).
'$1'(A,B):-a(A,C),s(C,B).
'$1'(A,B):-b(A,C),b(C,B).
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-'$1'(A,C),'$1'(C,B).
s(A,B):-'$1'(A,C),b(C,B).
s(A,B):-'$1'(A,C),s(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),a(C,B).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-b(A,C),b(C,B).
s(A,B):-s(A,C),'$1'(C,B).
s(A,B):-s(A,C),b(C,B).
s(A,B):-s(A,C),s(C,B).
==

Now, let's see how we can apply constraints to remove over-general
clauses from the output hypothesis.

To begin with, we can observe that most of the over-general clauses in
the learned hypothesis are recursive:

==
'$1'(A,B):-'$1'(A,C),b(C,B).
'$1'(A,B):-a(A,C),'$1'(C,B).
s(A,B):-'$1'(A,C),s(C,B).
s(A,B):-a(A,C),s(C,B).
s(A,B):-s(A,C),'$1'(C,B).
s(A,B):-s(A,C),b(C,B).
s(A,B):-s(A,C),s(C,B).
==

Remember that in the hypothesis learned at section 2, at the start of
this list of instructions, there are no recusive hypotheses. Here it is
again for ease of reference:

==
'$1'(A,B):-a(A,C),a(C,B).
'$1'(A,B):-a(A,C),s(C,B).
'$1'(A,B):-b(A,C),b(C,B).
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-'$1'(A,C),'$1'(C,B).
s(A,B):-'$1'(A,C),b(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).
==

Note well: the _unfolded_ hypothesis, seen e.g. in section 6 above,
includes a recursive clause. But not the hypothesis with invented
predicates!

If you have this kind of knowledge about your learning problem you can
use metarule constraints to remove unwanted clauss from the output
hypothesis.

You will find following three constraint declarations in this experiment
file. Uncomment them to apply their constraints:

==
configuration:metarule_constraints(m(chain,P,P,_),fail).
configuration:metarule_constraints(m(chain,P,_,P),fail).
configuration:metarule_constraints(m(chain,_,P,P),fail).
==

The three constraints above cut off instances of the Chain metarule
where any two of its second-order variables are the same, in other
words, they cut off recursive instances of Chain.

Make another learning attempt:

==
?- learn(s/2, _Ps), print_clauses(_Ps), length(_Ps,N).
'$1'(A,B):-a(A,C),s(C,B).
'$1'(A,B):-s(A,C),b(C,B).
s(A,B):-'$1'(A,C),b(C,B).
s(A,B):-a(A,C),'$1'(C,B).
s(A,B):-a(A,C),b(C,B).
N = 5.
==

The hypothesis above does not include any recursive clauses. All the
over-general clauses in the hypothesis learned without constraints were
recursive, so our hypothesis is now again correct.

Our new hypothesis is the same as the one learned with negative
examples, listed in Section 4, with the exception of one of the clauses
in Sub-Program 3 listed in that section:

==
s(A,B):-'$1'(A,C),'$1'(C,B).
==

This clause is recursive and so the metarule constraints we applied
exclude it from the output hypothesis. Remember that this clause leads
to the construction of an over-specialised clause that matches only the
example s([a,a,b,b],[]), when unfolded. By cutting out recursive clauses
to avoid over-generalisation, we managed to also avoid
over-specialisation. This will not always be the case!

We can unfold our hypothesis learned with constraints to verify that it
matches the target theory. You should now know how to do this:

==
% Set unfold_invented(true) and reduction(none) to see all learned clauses.
% :- auxiliaries:set_configuration_option(unfold_invented, [true]).
% :- auxiliaries:set_configuration_option(reduction, [none]).

?- learn(s/2).
s(A,B):-a(A,C),b(C,B).
s(A,B):-a(A,C),s(C,D),b(D,B).
true.
==


__9. Discussion__

__Recursion and Predicate Invention__

In the example of learning a^nb^n described in this experiment file we
have seen that in order to enable Louise's predicate invention we must
at least set the following configuration options:

==
clause_limit(K) % Where K > 1
max_invented(N)
==

Setting a clause limit higher than 1 allows Louise's Top Program
Construction algorithm to derive, from each single example, multiple
clauses that resolve with (or "call") each other and are instances of
one or more metarules. Clauses that resolve with each other may be
recursive or have invented predicates in their head or body. Or both. If
you want Louise to learn hypotheses with recursion and predicate
invention, you must set the value of clause_limit/1 to 2 or more.

The option max_invented(N) tells Louise to try and define at most N
invented predicates. Invented predicates are predicates that are not
defined in the background knowledge and for whom we do not have
examples. Without the ability for predicate invention, the user must
define all the necessary background knowledge for a learning target.


__Cardinality of the invented Top Program__

In all the examples above, we can note that the definition of the
invented predicate '$1' is a bit of a jumble. Ideally, we'd like to have
three separate invented predicates, one for each sub-program. That way
we wouldn't need to use metarule constraints to shape the Top
Program- we could simply adjust the max_invented/1 option, to allow
learning more or fewer invented predicates.

Unfortunately this is not easy to do. The definition of the invented
predicate symbol '$1' is the Top Program for that predicate given the
elements of the MIL problem. A Top Program is the set of _all_ clauses
that are instances of the metarules and that entail each example with
respect to the background knowledge.

In other words, the Top Program is "all the ways to skin the cat". This
verbosity is characteristic of Louise's Top Program Construction
algorithm and it is also, counter-intuitively, at the root of its
improved efficiency compared to search-based procedures. It turns out
it's faster to directly construct all clauses in correct hypotheses than
it is to search the space of hypotheses for any one subset thereof
substituting one of many correct hypotheses. That is, unless the
Hypothesis Space includes a very low number of hypotheses.

*/

%configuration:metarule_constraints(m(chain,P,P,_),fail).
%configuration:metarule_constraints(m(chain,P,_,P),fail).
%configuration:metarule_constraints(m(chain,_,P,P),fail).

:- auxiliaries:set_configuration_option(clause_limit, [3]).
:- auxiliaries:set_configuration_option(max_invented, [1]).
%:- auxiliaries:set_configuration_option(unfold_invented, [true]).
%:- auxiliaries:set_configuration_option(reduction, [none]).

background_knowledge(s/2,[a/2,b/2]).

metarules(s/2,[chain]).

positive_example(s/2,E):-
% Uncomment extra examples to experiment with different combinations
% thereof.
	member(E, [%s([a,b],[])
		  s([a,a,b,b],[])
		  %,s([a,a,a,b,b,b],[])
		  %,s([a,a,a,a,b,b,b,b],[])
		  %,s([a,a,a,a,a,b,b,b,b,b],[])
		  %,s([a,a,a,a,a,a,a,b,b,b,b,b,b,b],[])
		  ]).

negative_example(s/2,E):-
	member(E,[s([a,a],[])
		 ,s([b,b],[])
		 ,s([a,a,b],[])
		 ,s([a,b,b],[])
	       ]).

a([a|T],T).
b([b|T],T).
