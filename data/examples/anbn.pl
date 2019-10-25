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

1. Set an appropriate limit for the number of invented predicates in
   the configuration:

==
max_invented(1).
==

2. Call learn_dynamic/1 to use dynamic learning:

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

Dynamic learning proceeds in episodes, where the hypothesis learned in
episode k is added to the background knowledge for episode k+1.

Additionally, in each episode, the original set of metarules is
extended, then, if any extended metarules are successfully used to
generalise an example, the metasubstitutions of the _original_ metarules
in an extension pair are added to the Top program.

This results in a set of connected clauses that are otherwise impossible
to learn given the original background knowledge and metarules.

The following is an example of non-dynamic learning, with only the chain
metarule and three examples of a^nb^n strings given:

==
?- learn('S'/2).
'S'([a,a,a,b,b,b],[]).
'S'([a,a,b,b],[]).
'S'(A,B):-'A'(A,C),'B'(C,B). [1]
true.
==

Above, the only clause of 'S'/2 that is learned is a clause generalising
the first example, 'S'([a,b],[]). The chain metarule and the background
definitions of 'A'/2 and 'B'/2 are not sufficient to construct a
hypothesis that entails any more examples.

With dynamic learning, the clause in [1] is learned in the first
episode, then added to the background knowledge for the second episode.
In the second episode, the chain metarule is extended and used in Top
program construction, resulting in the following clause:

==
'S'(A,B):-'A'(A,C),'S'(C,D),'B'(D,B). [2]
==

Finally, the clause in [2] is discarded and the instances of chain used
in the extension step are added to the leared hypothesis. An invented
predicate symbol, 'S_1'/2 is used to connect the two clauses:

==
'S'(A,B):-'S_1'(A,C),'B'(C,B).
'S_1'(A,B):-'A'(A,C),'S'(C,B).
==

Refer to the documentation of the dynamic_learning.pl module for a more
in depth explanation of the dynamic learning procedure.


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

*/

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
