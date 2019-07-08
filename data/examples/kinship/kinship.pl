:-module(kinship, [background_knowledge/2
		  ,metarules/2
		  ,positive_example/2
		  ,negative_example/2
		  ,parent/2
		  ,child/2
		  ,son/2
		  ,daughter/2
		  ,brother/2
		  ,sister/2
		  ,father/2
		  ,mother/2
		  ,male/1
		  ,female/1
		  ]).


%configuration:metarule(postcorn,P,Q,R):- m(P,X,Y), m(Q,X,Y), m(R,X).

background_knowledge(blood_relative/2,[parent/2
				      ,child/2
				      ,son/2
				      ,daughter/2
				      ,brother/2
				      ,sister/2
				      ,father/2
				      ,mother/2
				      ,male/1
				      ,female/1
				      ]).

background_knowledge(relative/2,[parent/2
				,child/2
				,son/2
				,daughter/2
				,brother/2
				,sister/2
				,father/2
				,mother/2
				,male/1
				,female/1
				]).


metarules(blood_relative/2,[identity]).
metarules(relative/2,[chain]).

positive_example(blood_relative/2,blood_relative(X,Y)):-
	blood_relative(X,Y).
positive_example(relative/2,relative(X,Y)):-
	relative(X,Y).

negative_example(blood_relative/2,_):-
	fail.
negative_example(relative/2,_):-
	fail.

% Target theory for relative/2.
% A relative is a blood relative of a blood relative who is not him- or
% her-self a blood relative. So a non-blood relative.
relative(X,Y):-
	blood_relative(X,Z)
	,blood_relative(Z,Y)
	,\+ blood_relative(X,Y).

% Target theory for blood relative/2.
% This one's unreduced, actually.
blood_relative(X,Y):- parent(X,Y).
blood_relative(X,Y):- child(X,Y).
blood_relative(X,Y):- son(X,Y).
blood_relative(X,Y):- daughter(X,Y).
blood_relative(X,Y):- brother(X,Y).
blood_relative(X,Y):- sister(X,Y).
blood_relative(X,Y):- father(X,Y).
blood_relative(X,Y):- mother(X,Y).

parent(X,Y):- father(X,Y).
parent(X,Y):- mother(X,Y).
child(X,Y):- son(X,Y).
child(X,Y):- daughter(X,Y).
son(X,Y):- male(X),parent(Y,X).
daughter(X,Y):- female(X),parent(Y,X).
brother(X,Y):- son(X,Z), parent(Z,Y), X \= Y.
sister(X,Y):- daughter(X,Z), parent(Z,Y), X \= Y.

father(stathis, kostas).
father(stathis, gioula).
father(stefanos, miltos).
father(stefanos, akis).
father(stefanos, theodora).
father(kostas, stassa).
father(miltos, stefanakis).
father(akis, kostis).
father(vassilis, nikolas).
father(vassilis, alexandros).

mother(alexandra, kostas).
mother(alexandra, gioula).
mother(voula, miltos).
mother(voula, akis).
mother(voula, theodora).
mother(ada, stefanakis).
mother(theodora, stassa).
mother(efi, kostis).
mother(gioula, nikolas).
mother(gioula, alexandros).

male(stathis).
male(stefanos).
male(kostas).
male(vassilis).
male(akis).
male(miltos).
male(stefanakis).
male(nikolas).
male(kostis).
male(alexandros).

female(alexandra).
female(voula).
female(theodora).
female(gioula).
female(ada).
female(efi).
female(stassa).

/*father(stathis, kostas).
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
*/
