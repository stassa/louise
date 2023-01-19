:-module(tiny_kinship, [background_knowledge/2
		       ,metarules/2
		       ,positive_example/2
		       ,negative_example/2
		       ,ancestor/2
		       ,grandparent/2
		       ,grandfather/2
		       ,grandmother/2
		       ,parent/2
		       ,husband/2
		       ,wife/2
		       ,child/2
		       ,son/2
		       ,daughter/2
		       ,father/2
		       ,mother/2
		       ,male/1
		       ,female/1
		       ]).

/** <module> Experiment file for a small family domain.

This experiment file declares multiple learning targets, each
representing one family relation. Most of those relations are defined in
terms of each other. The whole family tree is based on the extensional
definitions of father/2, mother/2 male/2 and female/2. It's a nuclear
family.


__Known good configuration__

Make sure your configuration option are set as follows. Important
options highlighted with an asterisk, "*":

==
?- list_config.
* clause_limit(0)
example_clauses(call)
* experiment_file(data/examples/tiny_kinship.pl,tiny_kinship)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
listing_limit(10)
* max_error(0,0)
* max_invented(0)
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

__List learning results__

Call list_learning_results/0 to show the results of learning all
learning targets defined in this experiment file. It should look like
this:

==
?- list_learning_results.
ancestor(A,B):-parent(A,B).
ancestor(A,B):-ancestor(A,C),ancestor(C,B).

grandparent(A,B):-parent(A,C),parent(C,B).

grandfather(A,B):-father(A,C),parent(C,B).
grandfather(A,B):-husband(A,C),grandmother(C,B).

grandmother(A,B):-mother(A,C),parent(C,B).

parent(A,B):-father(A,B).
parent(A,B):-mother(A,B).

husband(A,B):-father(A,C),mother(B,C).

wife(A,B):-mother(A,C),father(B,C).

child(A,B):-daughter(A,B).
child(A,B):-son(A,B).

son(A,B):-male(A),child(A,B).

daughter(A,B):-female(A),child(A,B).

father(stathis,kostas).
father(stefanos,dora).
father(kostas,stassa).
father(A,B):-male(A),parent(A,B).

mother(alexandra,kostas).
mother(paraskevi,dora).
mother(dora,stassa).
mother(A,B):-female(A),parent(A,B).

male(A,A):-male(A).

female(A,A):-female(A).

true.
==


*/

% Background knowledge declarations
background_knowledge(ancestor/2,[parent/2]).
background_knowledge(grandparent/2,[parent/2]).
background_knowledge(grandfather/2,[father/2,parent/2,husband/2,grandmother/2]).
% Alternative BK for grandfather/2
%background_knowledge(grandfather/2,[father/2,mother/2,parent/2]).
background_knowledge(grandmother/2,[mother/2,parent/2]).
background_knowledge(parent/2,[father/2,mother/2]).
background_knowledge(husband/2,[father/2,mother/2]).
background_knowledge(wife/2,[father/2,mother/2]).
% Alternative BK for child/2
%background_knowledge(child/2,[father/2,mother/2,parent/2]).
background_knowledge(child/2,[son/2,daughter/2]).
background_knowledge(son/2,[child/2,male/1]).
background_knowledge(daughter/2,[child/2,female/1]).
background_knowledge(father/2,[parent/2,male/1]).
background_knowledge(mother/2,[parent/2,female/1]).
background_knowledge(male/2,[male/1]).
background_knowledge(female/2,[female/1]).

% Metarules
metarules(ancestor/2,[tailrec,identity]).
metarules(grandparent/2,[chain]).
metarules(grandfather/2,[chain]).
metarules(grandmother/2,[chain]).
metarules(parent/2,[identity]).
metarules(husband/2,[switch]).
metarules(wife/2,[switch]).
% Goes with the alternative BK.
%metarules(child/2,[inverse]).
metarules(child/2,[identity]).
metarules(son/2,[precon]).
metarules(daughter/2,[precon]).
metarules(father/2,[precon,projection_21]).
metarules(mother/2,[precon,projection_21]).
metarules(male/2,[identity,projection_21]).
metarules(female/2,[identity,projection_21]).

% Positive and negative examples generators.
positive_example(ancestor/2,ancestor(A,B)):-
	ancestor(A,B).
positive_example(grandparent/2,grandparent(A,B)):-
	grandparent(A,B).
positive_example(grandfather/2,grandfather(A,B)):-
	grandfather(A,B).
positive_example(grandmother/2,grandmother(A,B)):-
	grandmother(A,B).
positive_example(parent/2,parent(A,B)):-
	parent(A,B).
positive_example(husband/2,husband(A,B)):-
	husband(A,B).
positive_example(wife/2,wife(A,B)):-
	wife(A,B).
positive_example(child/2,child(A,B)):-
	child(A,B).
positive_example(son/2,son(A,B)):-
	son(A,B).
positive_example(daughter/2,daughter(A,B)):-
	daughter(A,B).
positive_example(father/2,father(A,B)):-
	father(A,B).
positive_example(mother/2,mother(A,B)):-
	mother(A,B).
positive_example(male/2,male(A,A)):-
	male(A).
positive_example(female/2,female(A,A)):-
	female(A).

negative_example(ancestor/2,ancestor(A,B)):-
	ancestor(B,A).
negative_example(grandparent/2,grandparent(A,B)):-
	grandparent(B,A).
negative_example(grandfather/2,grandfather(A,B)):-
	grandmother(A,B).
negative_example(grandmother/2,grandmother(A,B)):-
	grandfather(A,B).
negative_example(parent/2,parent(A,B)):-
	parent(B,A).
negative_example(husband/2,husband(A,B)):-
	wife(A,B).
negative_example(wife/2,wife(A,B)):-
	husband(A,B).
negative_example(child/2,child(A,B)):-
	parent(A,B).
negative_example(son/2,son(A,B)):-
	daughter(A,B).
negative_example(daughter/2,daughter(A,B)):-
	son(A,B).
negative_example(father/2,father(A,B)):-
	mother(A,B).
negative_example(mother/2,mother(A,B)):-
	father(A,B).
negative_example(male/2,male(A,A)):-
	female(A).
negative_example(female/2,female(A,A)):-
	male(A).


% Background knowledge definitions
ancestor(X,Y):-
	parent(X,Y).
ancestor(X,Y):-
	parent(X,Z)
	,ancestor(Z,Y).

grandparent(X,Y):-
	grandfather(X,Y).
grandparent(X,Y):-
	grandmother(X,Y).

grandfather(A,B):-
	father(A,C)
	,parent(C,B).

grandmother(A,B):-
	mother(A,C)
	,parent(C,B).

parent(X, Y):-
	father(X,Y).
parent(X, Y):-
	mother(X,Y).

husband(X,Y):-
	father(X,Z)
	,mother(Y,Z).

wife(X,Y):-
	mother(X,Z)
	,father(Y,Z).

child(X,Y):-
	parent(Y,X).

/*
% This alternative definition of child/2 causes learning child/2 with
% son/2 and daughter/2 as background knowledge to go into an infinite
% _right_ recursion! That's because it is mutually recursive with its
% background predicates. Don't do this!

child(X,Y):-
	son(X,Y).
child(X,Y):-
	daughter(X,Y).
*/

son(X,Y):-
	male(X)
	,child(X,Y).

daughter(X,Y):-
	female(X)
	,child(X,Y).

father(stathis, kostas).
father(stefanos, dora).
father(kostas, stassa).

mother(alexandra, kostas).
mother(paraskevi, dora).
mother(dora, stassa).

male(stathis).
male(stefanos).
male(kostas).

female(dora).
female(stassa).
female(alexandra).
female(paraskevi).
