:-module(robots_gen, [background_knowledge/2
		     ,metarules/2
		     ,positive_example/2
		     ,negative_example/2
		     ]).

:-user:use_module('../world.pl').
:-user:use_module('../render.pl').
:-user:use_module(move_generator).
:- dataset_file_name(_,Fn)
  ,reexport(Fn).

:-dynamic m/3
         ,m/2.

configuration:metarule(unit_identity,P,Q):- m(P,X,_Y), m(Q,X).
configuration:metarule(unit_identity, [P,Q], [X,Y], mec(P,X,Y) :- mec(Q,X)).

background_knowledge(move/2, [move_right/2
			     ,move_left/2
			     ,move_up/2
			     ,move_down/2
			     ,start/1
			     ,end/1
			  ]).


metarules(move/2,[unit_identity,chain,postcon,projection]).

positive_example(move/2,move(Ss,Gs)):-
	dataset_file_name(Bn,_)
	,Bn:move(Ss,Gs).

negative_example(move/2,_):-
	fail.
