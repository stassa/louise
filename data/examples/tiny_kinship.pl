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
		       ,father/2
		       ,mother/2
		       ,male/1
		       ,female/1
		       ]).

/** <module> Experiment file for a small family domain.

Includes examples for ancestor/2, father/2, grandfather/2 and male/2.

*/

% Background knowledge declarations
background_knowledge(ancestor/2,[father/2,mother/2,parent/2]).
background_knowledge(grandparent/2,[father/2,mother/2,parent/2]).
background_knowledge(grandfather/2,[father/2,mother/2,parent/2]).
%background_knowledge(grandfather/2,[father/2,mother/2,husband/2,grandmother/2,parent/2]).
background_knowledge(grandmother/2,[father/2,mother/2,parent/2]).
background_knowledge(parent/2,[father/2,mother/2]).
background_knowledge(husband/2,[father/2,mother/2]).
background_knowledge(wife/2,[father/2,mother/2]).
background_knowledge(father/2,[father/2,mother/2,parent/2,male/1]).
background_knowledge(mother/2,[father/2,mother/2,parent/2,female/1]).
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
metarules(father/2,[precon,projection]).
metarules(mother/2,[precon,projection]).
metarules(male/2,[identity,projection]).
metarules(female/2,[identity,projection]).

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
negative_example(father/2,father(A,B)):-
	mother(A,B).
negative_example(mother/2,mother(A,B)):-
	father(A,B).
negative_example(male/2,male(A,A)):-
	female(A).
negative_example(female/2,female(A,A)):-
	male(A).
negative_example(husband/2,husband(A,B)):-
	wife(A,B).
negative_example(wife/2,wife(A,B)):-
	husband(A,B).


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
