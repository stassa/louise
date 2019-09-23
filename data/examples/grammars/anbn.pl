:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,'A'/2
	       ,'B'/2
	       ]).

/** <module> Experiment file for a^nb^n grammar.

This example experiment file illustrates the use of episodic learning
and metarule extension to learn a hypothesis that can't otherwise be
learned from the elements of the MIL problem as given by the user.

This experiment file also demonstrates the use of the auxiliary
predicate set_configuration_option/2 to dynamically manipulate
configuration options.

Learning query
==============

1. episodic_learning/1 performs episodic learning:

==
?- learn_episodic('S'/2).
'S'(A,B):-'A'(A,C),'B'(C,B).
'S'(A,B):-'A'(A,C),'S'(C,D),'B'(D,B).
true.
==

2. Note that the configuration option extend_metarules/1 is set
dynamically using set_configuration_option/2 as a directive (at the
start of the source code, below):

==
:- set_configuration_option(extend_metarules, [1]).
==
*/

:- set_configuration_option(extend_metarules, [1]).

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
