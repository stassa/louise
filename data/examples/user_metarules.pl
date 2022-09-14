:-module(user_metarules, [background_knowledge/2
			 ,metarules/2
			 ,positive_example/2
			 ,negative_example/2
			 ,q/2
			 ,r/2
			 ]).

/** <module> Example of	user-defined metarules.

A simple example demonstrating how to define your own metarules.

__Declare new metarules__

New metarules are defined in the configuration module. The predicate
metarule/2 is multifile so metarules can also be declared in experiment
file modules.

metarule/2 is declared as an operator in the configuration module.
Hence, we must load the configuration module to avoid existence
errors.

Therefore, add this directive at the top of any experiment file that
defines its own metarules:

==
:-use_module(configuration).
==

Each metarule defined in an experiment file must have a module qualifier
"configuration:", to add the metarule to the configuration module:

==
configuration:special_chain metarule 'P(x,y):- Q(x,z), R(z,y)'.
==

That's it. Your metarule is now defined. You can use it to learn.


__List the elements of the learning problem__

Belwo we've added a couple of simple exampels and background definitions
to learn a simple hypothesis:

==
?- list_mil_problem(p/2).
Positive examples
-----------------
p(a,b).

Negative examples
-----------------
[]

Background knowledge
--------------------
q/2:
q(a,c).

r/2:
r(c,b).

Metarules
---------
(Special-chain) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(x,z),R(z,y)

true.
==

If the user metarule is defined correctly it should be printed out in
the bottom of the listing of the elements of the MIL problem, as you
see above.


__Make a learning attempt__

It should look like this:

==
?- list_learning_results.
p(A,B):-q(A,C),r(C,B).

true.
==

Note that the learned clause is an instance of the metarule defined in
this experiment file.

*/

% metarule/2 is declared as an operator in the configuration module.
% Hence, we must load the configuration module to avoid existence
% errors.
:-use_module(configuration).

% Each metarule defined in an experiment file must have a module
% qualifier "configuration:", to make them part of the configuration
% module.
configuration:special_chain metarule 'P(x,y):- Q(x,z), R(z,y)'.

background_knowledge(p/2, [q/2,r/2]).

metarules(p/2,[special_chain]).

positive_example(p/2,p(a,b)).

negative_example(p/2,_):-
	fail.

q(a,c).
r(c,b).
