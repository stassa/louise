:-module(anbn, [background_knowledge/2
	       ,metarules/2
	       ,positive_example/2
	       ,negative_example/2
	       ,'A'/2
	       ,'B'/2
	       ]).

/** <module> Experiment file for a^nb^n grammar.
*/

% This is not picked up if I define it here. I'll need to fix that.
% That _too_.
% metarule(long_chain_3,P,Q,R,S):- m(P,X,Y),m(Q,X,Z),m(R,Z,V),m(S,V,Y).

background_knowledge('S'/2,['A'/2,'B'/2]).

metarules('S'/2,[long_chain_3,chain]).

positive_example('S'/2,E):-
	member(E, ['S'([a,b],[])
		  ,'S'([a,a,b,b],[])
		  ,'S'([a,a,a,b,b,b],[])
		  ]).

negative_example('S'/2,_):-
	fail.

'A'([a|A], A).
'B'([b|A], A).
