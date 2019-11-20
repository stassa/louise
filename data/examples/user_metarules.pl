:-module(user_metarules, [background_knowledge/2
			 ,metarules/2
			 ,positive_example/2
			 ,negative_example/2
			 ,q/2
			 ,r/2
			 ]).

% Not strictly necessary but suppresses errors in IDE.
:-use_module(configuration).

/** <module> Example of	user-defined metarules.

*/

% Each metarule defined in an experiment file must have a module
% qualifier "configuration:", to make them part of the configuration
% module.
configuration:special_chain metarule 'P(x,y):- Q(x,z), R(z,y)'.
% This returns something weird.
%configuration:special_chain metarule 'P(x,y):- Q(x,z)'.

background_knowledge(p/2, [q/2,r/2]).

metarules(p/2,[special_chain]).

positive_example(p/2,p(a,b)).

negative_example(p/2,_):-
	fail.

q(a,c).
r(c,b).
