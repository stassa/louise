:-module(hero_generator, [write_dataset/0
			 ,heroes/1
			 ]).

:-use_module(heroes_configuration).

/** <module> Generator for heroes data.

Usage
=====

1. Edit heroes_configuration.pl to set the attributes that heroes will
have.

==
hero_attributes([class,honour,faith,kills]).
==

Currently known hero attribute are: [class, race, honour, wealth, kills,
faith], defined in this file. Any subset of this set will do. There is
no attriute that is mandatory, however the alignment.pl file declares a
target theory that expects certain attributes to be available.

2. Write a new dataset with the following query:

==
?- hero_generator:write_dataset.
==

3. Remember to reload everything afterwards. You can use make/0:

==
?- make.
==

4. Edit the target theory used to generate positive and negative
examples in alignment.pl

Other code in alignment.pl, besides the target theory, takes the names
of attributes in the generated dataset file from the exports of
heroes_configuration.pl. For example, that is the case with
hero_attributes/2. Ensure the generated dataset file is up to date with
the settings in heroes_configuration.pl before running experiments.

*/


%!	write_dataset is det.
%
%	Write a generated heroes' dataset to file.
%
%	The path to the dataset file is determined by the configuration
%	option dataset_filename/1.
%
%	Attributes of heroes in the generated dataset are determined by
%	the configuration option hero_attributes/1.
%
write_dataset:-
	dataset_filename(P)
	,hero_attributes(As)
	,findall(A/2,member(A,As),Es)
	,heroes(Hs)
	,flatten(Hs, Hs_f)
	,sort(Hs_f, Hs_s)
	,open(P,write,S,[alias(heroes_dataset)])
	,format(S,':-module(heroes, ~w).~n~n',[Es])
	,write_atoms(S,Hs_s)
	,close(S).


%!	write_atoms(+Stream,+Atoms) is det.
%
%	Write a list of atoms to a Stream.
%
write_atoms(S,As):-
	forall(member(A,As)
	      ,write_term(S,A,[fullstop(true)
			      ,nl(true)
			      ]
			 )
	      ).


%!	heroes(-Heroes) is det.
%
%	Generate a set of individual heroes.
%
%	Each hero is a unique combination of attributes, each
%	assossicated to a unique Id.
%
heroes(Hs):-
	% Select attributes.
	hero_attributes(As)
	% Create attribute terms (non-ground).
	,findall(T
		,(member(A,As)
		 ,functor(T,A,1)
		 )
		,Ts)
	% Instantiate attribute terms.
	,findall(Ts
	       ,(instantiate(Ts)
		)
	       ,As_)
	% Add hero ids to attribute terms.
	,findall(Hs_Id
		,(nth1(Id,As_,As_Id)
		 ,findall(A_Id_
			 ,(member(A_Id, As_Id)
			  ,A_Id =.. [F,A]
			  ,A_Id_ =.. [F,Id,A]
			  )
			 ,Hs_Id)
		 )
		,Hs).


%!	instantiate(:Attributes) is nondet.
%
%	Instantiate a list of heroes' Attributes.
%
instantiate([]):-
	!.
instantiate([A|As]):-
	call(A)
	,instantiate(As).



%!	class(?Class) is semidet.
%
%	A hero Class.
%
%	Describes a hero's chosen profession.
%
class(knight).
%class(soldier).
class(raider).
%class(spy).
class(priest).
%class(scholar).
%class(engineer).
%class(alchemist).


%!	race(?Race) is semidet.
%
%	A person's Race.
%
%	Describes the species of a hero.
%
race(human).
race(dwarf).
%race(troll).
%race(goblin).
race(orc).
race(elf).
%race(dark_elf).
%race(gnome).


%!	honour(?Level) is semidet.
%
%	A hero's Level of honour.
%
honour(honourable).
honour(decent).
honour(dishonourable).


%!	wealth(?Wealth) is semidet.
%
%	A person's Wealth.
%
%	Describes the richess amassed by a hero in the course of
%	exercising their chosen profession.
%
wealth(rich).
wealth(well_off).
wealth(poor).


%!	kills(?Kills) is semidet.
%
%	A heroe's Kills count.
%
%	How many enemies (or friends) a hero has killed.
%
kills(scourge).
kills(merciful).
kills(pacifist).


%!	faith(?Faith) is semidet.
%
%	A hero's Faith.
%
%	How strong a hero's faith is to their deity of choice.
%
faith(faithful).
faith(lukewarm).
faith(faithless).

