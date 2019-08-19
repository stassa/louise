:-module(special_metarules, [background_knowledge/2
			    ,metarules/2
			    ,positive_example/2
			    ,negative_example/2
			    ,q/2
			    ,r/2
			    ]).

/** <module> Example of	declaring per-experiment file metarules.

*/

% Each m/n predicate of a literal in the body of a metarule defined in
% an experiment file must be declared dynamic.
%
:-dynamic m/3.

% Each metarule defined in an experiment file must have a module
% qualifier "configuration:", to make them part of the configuration
% module.
%
configuration:metarule(special_chain,P,Q,R):- m(P,X,Y), m(Q,X,Z), m(R,Z,Y).

background_knowledge(p/2, [q/2,r/2]).

metarules(p/2,[special_chain]).

positive_example(p/2,p(a,b)).

negative_example(p/2,_):-
	fail.

q(a,c).
r(c,b).
