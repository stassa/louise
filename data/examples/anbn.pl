:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,'A'/2
	       ,'B'/2
	       ]).

/** <module> Experiment file for a^nb^n grammar.

This example experiment file illustrates the use of dynamic learning and
predicate invention to learn a hypothesis that can't otherwise be
learned from the elements of the MIL problem as given by the user.

Usage instructions
------------------

1. Make sure an appropriate limit is set for the number of invented
   predicates with a set_configuration_option/1 directive, as follows:

==
:- auxiliaries:set_configuration_option(max_invented, [1]).
==

The directives above should already be in this experiment file.

2. Call learn_dynamic/1 to use dynamic learning:

==
?- learn_dynamic('S'/2).
'$1'(A,B):-'S'(A,C),'B'(C,B).
'S'(A,B):-'A'(A,C),'$1'(C,B).
'S'(A,B):-'A'(A,C),'B'(C,B).
true.
==

*/

:- auxiliaries:set_configuration_option(max_invented, [1]).

% Tells list_learning_results/0 to use the right learning predicate.
auxiliaries:learning_predicate(learn_dynamic/1).

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
