:-module(kinship, [background_knowledge/2
		  ,metarules/2
		  ,positive_example/2
		  ,negative_example/2
		  ,grandparent/2
		  ,grandfather/2
		  ,grandmother/2
		  ,parent/2
		  ,married/2
		  ,aunt/2
		  ,uncle/2
		  ,cousin/2
		  ,nephew/2
		  ,niece/2
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


/* For experiments with Louise, when learning blood_relative/2, set:
   derivation_depth(9).
   resolutions(250_000).

Derivation depth is a bit more forgiving, but a high number of
resolutions is needed to get the best reduction results in the minimum
amount of time. Actually, 250,000 are not sufficient to fully reduce the
learned hypothesis- there is still some redundancy left. To be honest, I
don't know how high resolutions/1 should go to fully reduce the theory.
Bit of a weakness of the procedure there :/

*/

background_knowledge(blood_relative/2,[%grandparent/2
				      grandfather/2
				      ,grandmother/2
				      ,parent/2
				      ,aunt/2
				      ,uncle/2
				      ,cousin/2
				      ,nephew/2
				      ,niece/2
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

background_knowledge(relative/2,[grandfather/2
				,grandmother/2
				,parent/2
				,married/2
				,aunt/2
				,uncle/2
				,cousin/2
				,nephew/2
				,niece/2
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
metarules(relative/2,[identity,inverse,chain]).

positive_example(blood_relative/2,blood_relative(X,Y)):-
	blood_relative(X,Y).
positive_example(relative/2,relative(X,Y)):-
	relative(X,Y).

negative_example(blood_relative/2,blood_relative(X,Y)):-
	relative(X,Y).
negative_example(relative/2,relative(X,Y)):-
	blood_relative(X,Y).

% Target theory for relative/2.
% A relative is a blood relative of a blood relative who is not him- or
% her-self a blood relative. So a non-blood relative.
relative(X,Y):-
	blood_relative(X,Z)
	,blood_relative(Z,Y)
	,X \= Y
	,\+ blood_relative(X,Y)
	,\+ blood_relative(Y,X).


% Target theory for blood relative/2.
% This one's unreduced, actually.
% The reduction step throws some clauses out.
blood_relative(X,Y):- grandparent(X,Y).
blood_relative(X,Y):- grandfather(X,Y).
blood_relative(X,Y):- grandmother(X,Y).
blood_relative(X,Y):- parent(X,Y).
blood_relative(X,Y):- aunt(X,Y).
blood_relative(X,Y):- uncle(X,Y).
blood_relative(X,Y):- cousin(X,Y).
blood_relative(X,Y):- nephew(X,Y).
blood_relative(X,Y):- niece(X,Y).
blood_relative(X,Y):- child(X,Y).
blood_relative(X,Y):- son(X,Y).
blood_relative(X,Y):- daughter(X,Y).
blood_relative(X,Y):- brother(X,Y).
blood_relative(X,Y):- sister(X,Y).
blood_relative(X,Y):- father(X,Y).
blood_relative(X,Y):- mother(X,Y).

% BK definitions - blood relations
grandparent(X,Y):- grandfather(X,Y).
grandparent(X,Y):- grandmother(X,Y).
grandfather(X,Y):- father(X,Z), parent(Z,Y).
grandmother(X,Y):- mother(X,Z), parent(Z,Y).
parent(X,Y):- father(X,Y).
parent(X,Y):- mother(X,Y).
aunt(X,Y):- sister(X,Z),parent(Z,Y).
uncle(X,Y):- brother(X,Z),parent(Z,Y).
cousin(X,Y):-child(X,Z),aunt(Z,Y).
cousin(X,Y):-child(X,Z),uncle(Z,Y).
nephew(X,Y):- male(X),aunt(Y,X).
nephew(X,Y):- male(X),uncle(Y,X).
niece(X,Y):- female(X),aunt(Y,X).
niece(X,Y):- female(X),uncle(Y,X).
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

% BK definitions - not blood relations.
married(akis,efi).
married(kostas,theodora).
married(miltos,ada).
married(stathis,alexandra).
married(stefanos,voula).
married(vassilis,gioula).
% Extensional reflexion of the "married" relation
married(ada,miltos).
married(alexandra,stathis).
married(efi,akis).
married(gioula,vassilis).
married(theodora,kostas).
married(voula,stefanos).
