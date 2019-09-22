:-module(detect_evil, [background_knowledge/2
		      ,metarules/2
		      ,positive_example/2
		      ,negative_example/2]).

:-use_module(hero_generator).
:-use_module(heroes_configuration).
% Loads the generated heroes dataset if it exists
% or generates it and then loads it.
:- (exists_file('data/noise/heroes/heroes.pl')
  ->   reexport(heroes)
   ;   write_dataset
      ,reexport(heroes)
   ).

/** <module> Evil alignment detector for generated heroes dataset.

Hero data is loaded from heroes.pl.

See hero_generator.pl for instructions on how to generate hereos.pl.

*/

%!	noise(?Amount) is semidet.
%
%	The Amount of noise to add to positive examples.
%
%	"Noise" refers to classification noise and specifically noise
%	in the positive examples (i.e. negative examples classified
%	mistakenly as positive or in other words false positives).
%
%	Amount is a float, denoting the probability that a positive
%	example is replaced by a negative example, when calling
%	positive_example/2.
%
noise(0.1).

:-dynamic m/3.

configuration:metarule(double_identity,P,Q,R,Y,Z,D):-m(P,X,Y),m(Q,X,Z),m(R,X,D).

background_knowledge(alignment/2, BK):-
	heroes_configuration:hero_attributes(As)
	,findall(A/2
	       ,(member(A, As)
		)
	       ,BK).

metarules(alignment/2,[double_identity]).

% Positive examples generator with added noise.
positive_example(alignment/2,alignment(Id, evil)):-
	noise(P)
	,findall(alignment(Id, evil)
		,alignment(Id, evil)
		,Es)
	,p_list_partitions(P,Es,Ns,Cs)
	,length(Ns, N)
	,findall(E
		,negative_example(alignment/2,E)
		,Es_)
	,k_list_samples(N,Es_,Es_neg)
	,append(Cs,Es_neg,Es_all)
	,member(alignment(Id, evil), Es_all).

negative_example(alignment/2,alignment(Id, evil)):-
	hero_attributes(Id,_As)
	,\+ alignment(Id, evil).


% Target theory
alignment(Id,evil):- class(Id, knight), honour(Id, dishonourable).
alignment(Id,evil):- class(Id, raider), kills(Id, scourge).
alignment(Id,evil):- class(Id, priest), faith(Id, faithless).


%!	hero_attributes(?Id, ?Hero) is nondet.
%
%	A Heroe's attributes.
%
%	Hero is an atom hero/N, where N the number of hero attributes
%	declared in heroes_configuration.pl, as hero_attributes/1.
%
hero_attributes(Id,H):-
       heroes_configuration:hero_attributes([A|As])
       ,functor(Attr_1,A,2)
       ,Attr_1 =.. [A,Id,V]
       ,call(Attr_1)
       ,findall(V_
	       ,(member(A_, As)
		,functor(Attr,A_,2)
		,Attr =.. [A_,Id,V_]
		,call(Attr)
		)
	       ,Vs)
       ,length(As,N)
       ,N_ is N + 2 % id and value of first attr.
       ,functor(H, hero, N_)
       ,H =.. [hero,Id,V|Vs].
