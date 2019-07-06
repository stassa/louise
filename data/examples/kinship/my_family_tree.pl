:-module(my_family_tree, [background_knowledge/2
			 ,metarules/2
			 ,positive_example/2
			 ,negative_example/2
			 ,male/1
			 ,female/1
			 ,father/2
			 ,mother/2
			 ,parent/2
			 ,child/2
			 ,grandparent/2
			 ,great_grandparent/2
			 ,great_uncle/2
			 ,son/2
			 ,daughter/2
			 ,grandfather/2
			 ,grandmother/2
			 ,husband/2
			 ,wife/2
			 ,brother/2
			 ,sister/2
			 ,uncle_paternal/2
			 ,father_of_mother/2
			 ,aunt_paternal/2
			 ,mother_of_mother/2
			 ,uncle_maternal_maamaa/2
			 ,wife_of_maamaa/2
			 ,son_of_maamaa/2
			 ,daughter_of_maamaa/2
			 ,husband_of_daughter/2
			 ,father_of_wife/2
			 ,mother_of_wife/2
			 ,sons_wife/2
			 ,father_of_sons_wife/2
			 ,mother_of_sons_wife/2
			 ,daughters_husband/2
			 ,father_of_daughters_husband/2
			 ,mother_of_daughters_husband/2
			 ,fathers_sister_buwa/2
			 ,husband_of_buwa/2
			 ,daughter_of_buwa/2
			 ,son_of_buwa/2
			 ,brothers_sister/2
			 ,brothers_sisters_son/2
			 ,brothers_brother/2
			 ,brothers_brothers_son/2
			 ,brothers_sisters_daughter/2
			 ,brothers_brothers_daughter/2
			 ,sisters_sister/2
			 ,sisters_sisters_son/2
			 ,sisters_sisters_daughter/2
			 ,sisters_brother/2
			 ,sisters_brothers_son/2
			 ,sisters_brothers_daughter/2
			 ]).

% Names of all background predicates defined in this file.
background_knowledge([father/2
		     ,mother/2
		     ,male/1
		     ,female/1
		     ,parent/2
		     ,child/2
		     ,grandparent/2
		     ,great_grandparent/2
		     ,great_uncle/2
		     ,son/2
		     ,daughter/2
		     ,grandfather/2
		     ,grandmother/2
		     ,husband/2
		     ,wife/2
		     ,brother/2
		     ,sister/2
		     ,uncle_paternal/2
		     ,father_of_mother/2
		     ,aunt_paternal/2
		     ,mother_of_mother/2
		     ,uncle_maternal_maamaa/2
		     ,wife_of_maamaa/2
		     ,son_of_maamaa/2
		     ,daughter_of_maamaa/2
		     ,husband_of_daughter/2
		     ,father_of_wife/2
		     ,mother_of_wife/2
		     ,sons_wife/2
		     ,father_of_sons_wife/2
		     ,mother_of_sons_wife/2
		     ,daughters_husband/2
		     ,father_of_daughters_husband/2
		     ,mother_of_daughters_husband/2
		     ,fathers_sister_buwa/2
		     ,husband_of_buwa/2
		     ,daughter_of_buwa/2
		     ,son_of_buwa/2
		     ,brothers_sister/2
		     ,brothers_sisters_son/2
		     ,brothers_brother/2
		     ,brothers_brothers_son/2
		     ,brothers_sisters_daughter/2
		     ,brothers_brothers_daughter/2
		     ,sisters_sister/2
		     ,sisters_sisters_son/2
		     ,sisters_sisters_daughter/2
		     ,sisters_brother/2
		     ,sisters_brothers_son/2
		     ,sisters_brothers_daughter/2
		     ]).

% Background knowledge generator for every relation. The effect of this
% is that every relation is assigned every other relation as BK, except
% of course for any exceptions, defined first.
background_knowledge(grandfather/2,BK_):-
% We remove grandfather/2 from its BK because it's defined as the male
% grandfather and we don't want clauses of precon in the output for
% comparison with Thelma, where only chain clauses are needed.
	  !
	  ,background_knowledge(BK)
	  ,select(grandfather/2,BK,BK_).
background_knowledge(Relation,BK):-
	  nonvar(Relation)
	  ,background_knowledge(BK).


% Metarule declarations. Similar to BK, every releation is assigned
% every metarule, except for exceptions defined first.
metarules(grandfather/2,[chain]):-
	  !.
metarules(_,Ids):-
	  known_metarules(Ids).


% Generator for positive examples of any relation.
positive_example(Relation/Arity, Example):-
	  var(Relation)
	  ,background_knowledge(Relation/Arity, _BK)
	  ,functor(Example,Relation,Arity)
	  ,Example.
positive_example(Relation/Arity, Example):-
	  nonvar(Relation)
	  ,functor(Example,Relation,Arity)
	  ,Example.


% Generator for negative examples of any relation. These are examples of
% every other relation that is not the target relation, and such that
% the target relation does not "cover" the individuals in the example.
negative_example(Relation/Arity, Instance_):-
	  background_knowledge(BK)
	  ,(   var(Relation)
	   ->  member(Relation, BK)
	   ;   true
	   )
	  ,member(Relation_1/Arity,BK)
	  ,Relation_1 \= Relation
	  ,functor(Instance,Relation_1,Arity)
	  ,Instance
	  ,Instance =.. [_F1|Args]
	  ,functor(Instance_,Relation,Arity)
	  ,Instance_ =.. [_F2|Args]
	  ,\+ Instance_.


% Background definitions
father(stathis, kostas).
father(stathis, georgia).
father(stefanos, miltiadis).
father(stefanos, akis).
father(stefanos, theodora).
father(kostas, stassa).
father(miltiadis, stefanakis).
father(akis, kostis).
father(vassilis, nikolas).
father(vassilis, alexandros).

mother(alexandra, kostas).
mother(alexandra, georgia).
mother(paraskevi, miltiadis).
mother(paraskevi, akis).
mother(paraskevi, theodora).
mother(ada, stefanakis).
mother(theodora, stassa).
mother(efi, kostis).
mother(georgia, nikolas).
mother(georgia, alexandros).

male(stathis).
male(stefanos).
male(kostas).
male(vassilis).
male(akis).
male(miltiadis).
male(stefanakis).
male(nikolas).
male(kostis).
male(alexandros).

female(alexandra).
female(paraskevi).
female(theodora).
female(georgia).
female(ada).
female(efi).
female(stassa).

parent(A,B):-mother(A,B).
parent(A,B):-father(A,B).
child(A,B):-parent(B,A).
grandparent(A,B):-parent(A,C),parent(C,B).
great_grandparent(A,B):-parent(A,C),grandparent(C,B).
great_uncle(A,B) :- brother(A,C), grandparent(C,B).
son(A,B):-child(A,B),male(A).
daughter(A,B):-child(A,B),female(A).
grandfather(A,B):-grandparent(A,B),male(A).
grandmother(A,B):-grandparent(A,B),female(A).
husband(A,B):-father(A,C),mother(B,C).
wife(A,B):-mother(A,C),father(B,C).
brother(A,B):-son(A,C),mother(C,B).
sister(A,B):-daughter(A,C),mother(C,B).
uncle_paternal(A,B):-brother(A,C),father(C,B).
father_of_mother(A,B):-father(A,C),mother(C,B).
aunt_paternal(A,B):-sister(A,C),father(C,B).
mother_of_mother(A,B):-mother(A,C),mother(C,B).
uncle_maternal_maamaa(A,B):-brother(A,C),mother(C,B).
wife_of_maamaa(A,B):-wife(A,C),uncle_maternal_maamaa(C,B).
son_of_maamaa(A,B):-son(A,C),uncle_maternal_maamaa(C,B).
daughter_of_maamaa(A,B):-daughter(A,C),uncle_maternal_maamaa(C,B).
husband_of_daughter(A,B):-husband(A,C),daughter(C,B).
father_of_wife(A,B):-father(A,C),wife(C,B).
mother_of_wife(A,B):-mother(A,C),wife(C,B).
sons_wife(A,B):-wife(A,C),son(C,B).
father_of_sons_wife(A,B):-father(A,C),sons_wife(C,B).
mother_of_sons_wife(A,B):-mother(A,C),sons_wife(C,B).
daughters_husband(A,B):-husband(A,C),daughter(C,B).
father_of_daughters_husband(A,B):-father(A,C),daughters_husband(C,B).
mother_of_daughters_husband(A,B):-mother(A,C),daughters_husband(C,B).
fathers_sister_buwa(A,B):-sister(A,C),father(C,B).
husband_of_buwa(A,B):-husband(A,C),fathers_sister_buwa(C,B).
daughter_of_buwa(A,B):-daughter(A,C),fathers_sister_buwa(C,B).
son_of_buwa(A,B):-son(A,C),fathers_sister_buwa(C,B).
brothers_sister(A,B):-sister(A,C),brother(C,B).
brothers_sisters_son(A,B):-son(A,C),brothers_sister(C,B).
brothers_brother(A,B):-brother(A,C),brother(C,B).
brothers_brothers_son(A,B):-son(A,C),brothers_brother(C,B).
brothers_sisters_daughter(A,B):-daughter(A,C),brothers_sister(C,B).
brothers_brothers_daughter(A,B):-daughter(A,C),brothers_brother(C,B).
sisters_sister(A,B):-sister(A,C),sister(C,B).
sisters_sisters_son(A,B):-son(A,C),sisters_sister(C,B).
sisters_sisters_daughter(A,B):-daughter(A,C),sisters_sister(C,B).
sisters_brother(A,B):-brother(A,C),sister(C,B).
sisters_brothers_son(A,B):-son(A,C),sisters_brother(C,B).
sisters_brothers_daughter(A,B):-daughter(A,C),sisters_brother(C,B).

