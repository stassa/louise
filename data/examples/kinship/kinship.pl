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

background_knowledge(unrelated/2,[grandfather/2
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
metarules(unrelated/2,[identity,inverse,chain]).

positive_example(blood_relative/2,blood_relative(X,Y)):-
	blood_relative(X,Y).
positive_example(relative/2,E):-
	ground_relative(Rs)
	,member(E,Rs).
positive_example(unrelated/2,unrelated(X,Y)):-
	unrelated(X,Y).

negative_example(blood_relative/2,blood_relative(X,Y)):-
	unrelated(X,Y).
negative_example(relative/2,relative(X,Y)):-
	blood_relative(X,Y).
negative_example(unrelated/2,unrelated(X,Y)):-
	blood_relative(X,Y).


% Target theory for unrelated/2. Also used to generate negative examples
% for blood_relative/2.
unrelated(X,Y):-
	individual(X)
	,individual(Y)
	,\+ blood_relative(X,Y)
	,\+ blood_relative(Y,X)
	,\+ (blood_relative(X,Z)
	    ,blood_relative(Z,Y)
	    ).

individual(X):-
	male(X).
individual(X):-
	female(X).


% Target theory for relative/2.
% A relative is related by marriage to a blood relative
% or is a relative of a relative.
relative(X,Y):- married(X,Y).
relative(X,Y):- blood_relative(X,Z), married(Z,Y).
relative(X,Y):- married(X,Z),blood_relative(Z,Y).
relative(X,Y):- relative(X,Z), relative(Z,Y).

% Ground atoms of relative/2 obtained by bottom-up evaluation to avoid
% infinite recursion because of the left-recursive definition of
% relative/2.
ground_relative(Ls):-
	findall(blood_relative(X,Y)
	       ,blood_relative(X,Y)
	       ,Pos)
	,program(married/2,kinship,Ms)
	,append(Ms,Pos,Ps)
	,program(relative/2,kinship,Rs)
	,lfp_query(Rs,Ps,_Is,Ls).


% Target theory for blood relative/2.
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
%brother(X,Y):- son(X,Z), parent(Z,Y), distinct(X,Y).
%sister(X,Y):- daughter(X,Z), parent(Z,Y), distinct(X,Y).

%/*
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
%*/

/* Smaller problem for testing.
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

% BK definitions - not blood relations.
married(kostas,dora).
married(stathis,alexandra).
married(stefanos,paraskevi).
% Extensional reflexion of the "married" relation
married(alexandra,stathis).
married(dora,kostas).
married(paraskevi,stefanos).
*/

% Avoid using negation as failure to determine that two individuals are
% distinct.
distinct(stathis,stefanos).
distinct(stathis,kostas).
distinct(stathis,vassilis).
distinct(stathis,akis).
distinct(stathis,miltos).
distinct(stathis,stefanakis).
distinct(stathis,nikolas).
distinct(stathis,kostis).
distinct(stathis,alexandros).
distinct(stathis,alexandra).
distinct(stathis,voula).
distinct(stathis,theodora).
distinct(stathis,gioula).
distinct(stathis,ada).
distinct(stathis,efi).
distinct(stathis,stassa).
distinct(stefanos,stathis).
distinct(stefanos,kostas).
distinct(stefanos,vassilis).
distinct(stefanos,akis).
distinct(stefanos,miltos).
distinct(stefanos,stefanakis).
distinct(stefanos,nikolas).
distinct(stefanos,kostis).
distinct(stefanos,alexandros).
distinct(stefanos,alexandra).
distinct(stefanos,voula).
distinct(stefanos,theodora).
distinct(stefanos,gioula).
distinct(stefanos,ada).
distinct(stefanos,efi).
distinct(stefanos,stassa).
distinct(kostas,stathis).
distinct(kostas,stefanos).
distinct(kostas,vassilis).
distinct(kostas,akis).
distinct(kostas,miltos).
distinct(kostas,stefanakis).
distinct(kostas,nikolas).
distinct(kostas,kostis).
distinct(kostas,alexandros).
distinct(kostas,alexandra).
distinct(kostas,voula).
distinct(kostas,theodora).
distinct(kostas,gioula).
distinct(kostas,ada).
distinct(kostas,efi).
distinct(kostas,stassa).
distinct(vassilis,stathis).
distinct(vassilis,stefanos).
distinct(vassilis,kostas).
distinct(vassilis,akis).
distinct(vassilis,miltos).
distinct(vassilis,stefanakis).
distinct(vassilis,nikolas).
distinct(vassilis,kostis).
distinct(vassilis,alexandros).
distinct(vassilis,alexandra).
distinct(vassilis,voula).
distinct(vassilis,theodora).
distinct(vassilis,gioula).
distinct(vassilis,ada).
distinct(vassilis,efi).
distinct(vassilis,stassa).
distinct(akis,stathis).
distinct(akis,stefanos).
distinct(akis,kostas).
distinct(akis,vassilis).
distinct(akis,miltos).
distinct(akis,stefanakis).
distinct(akis,nikolas).
distinct(akis,kostis).
distinct(akis,alexandros).
distinct(akis,alexandra).
distinct(akis,voula).
distinct(akis,theodora).
distinct(akis,gioula).
distinct(akis,ada).
distinct(akis,efi).
distinct(akis,stassa).
distinct(miltos,stathis).
distinct(miltos,stefanos).
distinct(miltos,kostas).
distinct(miltos,vassilis).
distinct(miltos,akis).
distinct(miltos,stefanakis).
distinct(miltos,nikolas).
distinct(miltos,kostis).
distinct(miltos,alexandros).
distinct(miltos,alexandra).
distinct(miltos,voula).
distinct(miltos,theodora).
distinct(miltos,gioula).
distinct(miltos,ada).
distinct(miltos,efi).
distinct(miltos,stassa).
distinct(stefanakis,stathis).
distinct(stefanakis,stefanos).
distinct(stefanakis,kostas).
distinct(stefanakis,vassilis).
distinct(stefanakis,akis).
distinct(stefanakis,miltos).
distinct(stefanakis,nikolas).
distinct(stefanakis,kostis).
distinct(stefanakis,alexandros).
distinct(stefanakis,alexandra).
distinct(stefanakis,voula).
distinct(stefanakis,theodora).
distinct(stefanakis,gioula).
distinct(stefanakis,ada).
distinct(stefanakis,efi).
distinct(stefanakis,stassa).
distinct(nikolas,stathis).
distinct(nikolas,stefanos).
distinct(nikolas,kostas).
distinct(nikolas,vassilis).
distinct(nikolas,akis).
distinct(nikolas,miltos).
distinct(nikolas,stefanakis).
distinct(nikolas,kostis).
distinct(nikolas,alexandros).
distinct(nikolas,alexandra).
distinct(nikolas,voula).
distinct(nikolas,theodora).
distinct(nikolas,gioula).
distinct(nikolas,ada).
distinct(nikolas,efi).
distinct(nikolas,stassa).
distinct(kostis,stathis).
distinct(kostis,stefanos).
distinct(kostis,kostas).
distinct(kostis,vassilis).
distinct(kostis,akis).
distinct(kostis,miltos).
distinct(kostis,stefanakis).
distinct(kostis,nikolas).
distinct(kostis,alexandros).
distinct(kostis,alexandra).
distinct(kostis,voula).
distinct(kostis,theodora).
distinct(kostis,gioula).
distinct(kostis,ada).
distinct(kostis,efi).
distinct(kostis,stassa).
distinct(alexandros,stathis).
distinct(alexandros,stefanos).
distinct(alexandros,kostas).
distinct(alexandros,vassilis).
distinct(alexandros,akis).
distinct(alexandros,miltos).
distinct(alexandros,stefanakis).
distinct(alexandros,nikolas).
distinct(alexandros,kostis).
distinct(alexandros,alexandra).
distinct(alexandros,voula).
distinct(alexandros,theodora).
distinct(alexandros,gioula).
distinct(alexandros,ada).
distinct(alexandros,efi).
distinct(alexandros,stassa).
distinct(alexandra,stathis).
distinct(alexandra,stefanos).
distinct(alexandra,kostas).
distinct(alexandra,vassilis).
distinct(alexandra,akis).
distinct(alexandra,miltos).
distinct(alexandra,stefanakis).
distinct(alexandra,nikolas).
distinct(alexandra,kostis).
distinct(alexandra,alexandros).
distinct(alexandra,voula).
distinct(alexandra,theodora).
distinct(alexandra,gioula).
distinct(alexandra,ada).
distinct(alexandra,efi).
distinct(alexandra,stassa).
distinct(voula,stathis).
distinct(voula,stefanos).
distinct(voula,kostas).
distinct(voula,vassilis).
distinct(voula,akis).
distinct(voula,miltos).
distinct(voula,stefanakis).
distinct(voula,nikolas).
distinct(voula,kostis).
distinct(voula,alexandros).
distinct(voula,alexandra).
distinct(voula,theodora).
distinct(voula,gioula).
distinct(voula,ada).
distinct(voula,efi).
distinct(voula,stassa).
distinct(theodora,stathis).
distinct(theodora,stefanos).
distinct(theodora,kostas).
distinct(theodora,vassilis).
distinct(theodora,akis).
distinct(theodora,miltos).
distinct(theodora,stefanakis).
distinct(theodora,nikolas).
distinct(theodora,kostis).
distinct(theodora,alexandros).
distinct(theodora,alexandra).
distinct(theodora,voula).
distinct(theodora,gioula).
distinct(theodora,ada).
distinct(theodora,efi).
distinct(theodora,stassa).
distinct(gioula,stathis).
distinct(gioula,stefanos).
distinct(gioula,kostas).
distinct(gioula,vassilis).
distinct(gioula,akis).
distinct(gioula,miltos).
distinct(gioula,stefanakis).
distinct(gioula,nikolas).
distinct(gioula,kostis).
distinct(gioula,alexandros).
distinct(gioula,alexandra).
distinct(gioula,voula).
distinct(gioula,theodora).
distinct(gioula,ada).
distinct(gioula,efi).
distinct(gioula,stassa).
distinct(ada,stathis).
distinct(ada,stefanos).
distinct(ada,kostas).
distinct(ada,vassilis).
distinct(ada,akis).
distinct(ada,miltos).
distinct(ada,stefanakis).
distinct(ada,nikolas).
distinct(ada,kostis).
distinct(ada,alexandros).
distinct(ada,alexandra).
distinct(ada,voula).
distinct(ada,theodora).
distinct(ada,gioula).
distinct(ada,efi).
distinct(ada,stassa).
distinct(efi,stathis).
distinct(efi,stefanos).
distinct(efi,kostas).
distinct(efi,vassilis).
distinct(efi,akis).
distinct(efi,miltos).
distinct(efi,stefanakis).
distinct(efi,nikolas).
distinct(efi,kostis).
distinct(efi,alexandros).
distinct(efi,alexandra).
distinct(efi,voula).
distinct(efi,theodora).
distinct(efi,gioula).
distinct(efi,ada).
distinct(efi,stassa).
distinct(stassa,stathis).
distinct(stassa,stefanos).
distinct(stassa,kostas).
distinct(stassa,vassilis).
distinct(stassa,akis).
distinct(stassa,miltos).
distinct(stassa,stefanakis).
distinct(stassa,nikolas).
distinct(stassa,kostis).
distinct(stassa,alexandros).
distinct(stassa,alexandra).
distinct(stassa,voula).
distinct(stassa,theodora).
distinct(stassa,gioula).
distinct(stassa,ada).
distinct(stassa,efi).
