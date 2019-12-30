:-module(kin, [background_knowledge/2
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

background_knowledge(kin/2, [ancestor/2
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
                            ,female/1]).

metarules(kin/2,[chain,switch,precon]).

positive_example(kin/2,kin(A,B)):-
	kin(A,B)
	,A \= B.

negative_example(kin/2,kin(A,A)):-
	kin(A,A).

%/*
kin(A,B):- ancestor(A,B).
kin(A,B):- husband(A,B).
kin(A,B):- wife(A,B).
kin(A,B):- child(A,B).
%*/

/*
% Ancestor
kin(X,Y):- parent(X,Y).
kin(X,Y):- parent(X,Z) ,kin(Z,Y).
% Grandparent
kin(X,Y):- grandfather(X,Y).
kin(X,Y):- grandmother(X,Y).
% Grandfather
kin(A,B):- father(A,C), parent(C,B).
% Grandmother
kin(A,B):- mother(A,C), parent(C,B).
% Parent
kin(X, Y):- father(X,Y).
kin(X, Y):- mother(X,Y).
% Husband
kin(X,Y):- father(X,Z), mother(Y,Z).
% Wife
kin(X,Y):- mother(X,Z), father(Y,Z).
% Child
kin(X,Y):- parent(Y,X).
% Son
kin(X,Y):- male(X), child(X,Y).
% Daughter
kin(X,Y):- female(X), child(X,Y).
*/

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
