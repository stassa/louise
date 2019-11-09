:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,'A'/2
	       ,'B'/2
	       ]).

/** <module> Experiment file for a^nb^n grammar.

This example experiment file illustrates the use of _dynamic learning_
to learn a hypothesis that can't otherwise be learned from the elements
of the MIL problem as given by the user.

Usage instructions
------------------

1. Ensure an appropriate limit is set for the number of invented
   predicates with a set_configuration_option/1 directive, as follows:

==
:- auxiliaries:set_configuration_option(max_invented, [1]).
==

2. Ensure an appropriate limit to the number of dynamic generations is
   set, again with a set_configuration_option/1 directive:

==
:- auxiliaries:set_configuration_option(dynamic_generations, [1]).
==

The two directives above should already be in this experiment file.

3. Ensure appropriate metarule constraints are set to avoid
   left-recursions:

==
% McCarthyite metarule constraint: avoids left-recursion.
configuration:metarule_constraints(M,fail):-
	M =.. [m,_Id,P|Ps]
	,left_recursive(P,Ps).

left_recursive(T,[T|_Ps]):-
	!.
left_recursive(T,[T,T|_Ps]):-
	!.
left_recursive(T,[I,T|_Ps]):-
	atomic_list_concat([T,A],'_',I)
	,atom_number(A,_N).
==

The above constraint should already be declared in this experiment file.

4. Call learn_dynamic/1 to use dynamic learning:

==
?- learn_dynamic('S'/2).
'S'(A,B):-'A'(A,C),'B'(C,B).
'S'(A,B):-'S_1'(A,C),'B'(C,B).
'S_1'(A,B):-'A'(A,C),'S'(C,B).
true.
==

Dynamic learning
----------------

Dynamic learning is Louise's strategy for bias shift, by metarule
extension and predicate invention.

Dynamic learning proceeds in generations, in each of which the metarules
_Chain_ and _Inverse_ are each extended by the addition of a new body
literal.

In each dynamic learning generation a cycle of dynamic learning episodes
is performed. During a cycle of dynamic episodes the hypothesis learned
in episode k is added to the background knowledge for episode k+1.

Additionally, in each dynamic episode, the current generation of _Chain_
and _Inverse_ is extended by unfolding, then, if an extension resulting
from this unfolding is successfully used to generalise an example, the
metasubstitutions of the two _original_ metarules unfolded to produce
the extension are added to the Top program.

This results in a set of connected clauses that are otherwise impossible
to learn from the original background knowledge and metarules by Top
program construction alone.

A cycle of dynamic episodes ends when no new clauses can be added to the
learned hypothesis. Then a new generation of metarules is created, each
with one more body literal than the metarules in the previous
generation, and a new cycle of dynamic learning episodes begins, until
the current generation matches the limit set in the configuration option
dynamic_generations/1.

Refer to the documentation of the metarule extension module,
src(metagen), for details of metarule extension by generation and
unfolding.

Learning anbn by dynamic learning
--------------------------------

The MIL problem defined in this experiment file is as follows:

==
background_knowledge('S'/2,['A'/2,'B'/2]).

metarules('S'/2,[chain]).

positive_example('S'/2,E):-
	member(E, ['S'([a,b],[])
		  ,'S'([a,a,b,b],[])
		  ,'S'([a,a,a,b,b,b],[])
		  ]).

negative_example('S'/2,_):-
	fail.

% Definitions of background predicates
'A'([a|A], A).
'B'([b|A], A).
==

The following is an example of static learning with the MIL problem
defined in this experiment file:

==
% Result of learning anbn without dynamic learning
?- learn('S'/2).
'S'([a,a,a,b,b,b],[]).
'S'([a,a,b,b],[]).
'S'(A,B):-'A'(A,C),'B'(C,B). [1]
true.
==

In the result of the query listed above, the only clause of 'S'/2 that
is learned is [1], a clause generalising the first positive example of
anbn, 'S'([a,b],[]). This example is absent from the learned hypothesis
because it is entailed by [1], and so was discarded as redundant during
the Top program reduction step. The remaining two examples are included
in the learned hypothesis because they are not entailed by [1] and so
could not be discarded during Top program reduction. The chain metarule
and the background definitions of 'A'/2 and 'B'/2 are not sufficient to
construct a hypothesis that entails these two examples (or any other
strings of the anbn language, other than 'ab', for that matter).

When performing dynamic learning, the clause in [1] is learned in the
first dynamic learning episode of the first generation, then added to
the background knowledge for the second episode in the first generation.
In the second episode, the first-generation _Chain_ metarule is extended
by unfolding with itself and the first-generation _Inverse_. Then Top
program construction is performed with _Chain_, _Inverse_ and their
extensions by unfolding, resulting in the following clause:

==
% Clause learned in the second dynamic episode of the first dynamic
% learning generation.
'S'(A,B):-'A'(A,C),'S'(C,D),'B'(D,B). [2]
==

[2] is an instantiation of an extenstion by unfolding of the
first-generation _Chain_ with itself: it's an instance of a _Chain_
extension with three body literals (in fact, it's a second-generation
_Chain_). The new clause entails the two examples of anbn that were not
"covered" by the clauses in [1], learned in the previous episode.

Finally, the clause in [2] is discarded and the two copies of the
first-generation _Chain_ that were unfolded with each other in the
extension step are added to the leared hypothesis. An invented predicate
symbol, 'S_1'/2 is used to connect the two clauses:

==
'S'(A,B):-'S_1'(A,C),'B'(C,B). [3]
'S_1'(A,B):-'A'(A,C),'S'(C,B). [4]
==

The two clauses in [3] and [4] are added to the background knowledge and
a third dynamic episode is performed. However, no new clauses can be
learned from the augmented background knowledge and so dynamic learning
exits. Since a single-generation limit was imposed on dynamic learning,
the process exits with the learned hypothesis that now includes the
three clauses in [1], [3] and [4]:

==
'S'(A,B):-'A'(A,C),'B'(C,B).
'S'(A,B):-'S_1'(A,C),'B'(C,B).
'S_1'(A,B):-'A'(A,C),'S'(C,B).
==

Refer to the documentation of the dynamic_learning.pl module for a more
in depth explanation of the dynamic learning procedure.


Limiting the number of metarule generations
-------------------------------------------

The configuration option dynamic_generations/1 is set dynamically in
this experiment file with a directive calling
set_configuration_option/2. dynamic_generations/1 is set to 1 to force
the dynamic learning procedure to exit after a single generation:

==
:- auxiliaries:set_configuration_option(dynamic_generations, [1]).
==

Note that in this single generation a full cycle of dynamic learning
episodes is performed. These dynamic episodes all use the metarules in
the first generation of _Chain_ and _Inverse_.

If dynamic_generations/1 is set to a higher value dynamic learning will
enter an infinite recursion!

Avoiding infinite recursion during dynamic learning is something that
we're working on. For the time being, be warned that dynamic learning
can "go infinite" if too many dynamic generations are attempted, and
there is no way to know how many "too many" is up front.

Limiting predicate invention
----------------------------

The configuration option max_invented/1 is set dynamically in this
experiment file using set_configuration_option/2 as a directive (at the
start of the source code, below) to reduce the number of invented
predicates that are constructed, from all possible ones:

==
:- auxiliaries:set_configuration_option(max_invented, [1]).
==

With max_invented/1 set to something higher than 1, multiple invented
predicates can be constructed:

==
:- auxiliaries:set_configuration_option(max_invented, [2]).

?- learn_dynamic('S'/2).
'S'(A,B):-'A'(A,C),'B'(C,B).
'S'(A,B):-'A'(A,C),'S_2'(C,B).
'S'(A,B):-'S_1'(A,C),'B'(C,B).
'S_1'(A,B):-'A'(A,C),'S'(C,B).
'S_2'(A,B):-'S'(A,C),'B'(C,B).
true.
==

However, many of those are redundant, although not _logically_
redundant, which means they can't be reduced by Plotkin's program
reduction algorithm.

Like the number of dynamic generations, the maximum number of dynamic
predicates is, essentially, a hyperparameter of the dynamic learning
algorithm and has to be set carefully. This is another limitation of the
technique that we're working on addressing.

*/

% McCarthyite metarule constraint: avoids left-recursion.
configuration:metarule_constraints(M,fail):-
	M =.. [m,_Id,P|Ps]
	,left_recursive(P,Ps).

left_recursive(T,[T|_Ps]):-
	!.
left_recursive(T,[T,T|_Ps]):-
	!.
left_recursive(T,[I,T|_Ps]):-
	atomic_list_concat([T,A],'_',I)
	,atom_number(A,_N).

:- auxiliaries:set_configuration_option(dynamic_generations, [1]).
:- auxiliaries:set_configuration_option(max_invented, [1]).

background_knowledge('S'/2,['A'/2,'B'/2]).

metarules('S'/2,[chain]).

positive_example('S'/2,E):-
	member(E, ['S'([a,b],[])
		  ,'S'([a,a,b,b],[])
		  ,'S'([a,a,a,b,b,b],[])
		  ]).

negative_example('S'/2,_):-
	fail.

'A'([a|A], A).
'B'([b|A], A).
