:-module(move_generator, [write_dataset/0
			 ,dataset_file_name/2
			 ,generate_locations/2
			 ,generate_moves/1
			 ,generate_problems/1
			 ]).

:-use_module(generator_configuration).
:- (    configuration:learner(louise)
   ->	use_module(data(robots/world))
   ;    configuration:learner(thelma)
   ->   use_module(data(louise/robots/world))
   ).

/** <module> Data generator for grid world navigation experiments.

Predicates in this module generate extensional definitions of location
and move primitives, and grid world navigation tasks for a grid
world of a given type and dimensions.

The type and dimensions of the grid world are given in
experiment_world/1 and world_dimensions/2 respectively

Navigation tasks
================

Grid world navigation tasks consist of a list of atoms of the predicate
move/2 where the first argument is a start state and the second argument
is the end state of a grid world navigation task.

==
move([0/0,0/0,1-1],[0/0,0/0,1-1]).
move([0/0,0/1,1-1],[0/1,0/1,1-1]).
move([0/0,1/0,1-1],[1/0,1/0,1-1]).
% ...
==

Grid world states are generated by predicates in module _world_. Refer
to that module for an explanation of the representation of grid world
states. In general, the representation is a "vector" (a Prolog list)
listing the coordinates of locations on the grid world where the "robot"
(the grid world navigating agent) and various objects of interest, or
goals, are situated, plus the dimensions of the grid world, and
sometimes the name of the world also, for easy disambiguation between
states relevant to different worlds.

The predicate generate_problems/1 generates all possible move/2 tasks
for a given world.

Move primitives
===============

Move primitives consist of atoms of the predicates move_right/2,
move_left/2, move_up/2 and move_down/2, representing primitive grid
world navigation actions.

==
move_right([0/0,0/0,1-1],[1/0,0/0,1-1]).
move_left([1/0,0/0,1-1],[0/0,0/0,1-1]).
move_up([0/0,0/0,1-1],[0/1,0/0,1-1]).
move_down([0/1,0/0,1-1],[0/0,0/0,1-1]).
==

The predicate generate_moves/2 generates all atoms of the four primitive
actions that are true in a given world, or in other words, an
extensional definition of each of the four primitive actions.

Location primitives
===================

Location primitives consist of atoms of the predicates start/1 and
end/1 representing the start and end locations of all possible
navigation tasks in a grid world. start/1 atoms have as their single
argument each of the starting states of the move/2 atoms generated by
generate_problems/1. end/1 atoms have as their single argument each of
the end states of the move/2 atoms generated by generate_problems/2.

==
% Task:
move([0/0,0/0,1-1],[0/0,0/0,1-1]).

% Start and end locations:
start([0/0,0/0,1-1]).
end([0/0,0/0,1-1]).
==

Location primitives are generated by the predicate generate_locations/2.
This takes as input a list of move/2 atoms generated by
generate_problems/1 and outputs a list of the corresponding start/1 and
end/1 atoms.

Usage instructions
==================

Configuration options
---------------------

Before generating tasks and primitives, make sure you have the correct
options configured in generator_configuration.pl. The parameters you
absolutely need to set are the type of world and its dimensions. The
following options will choose an empty world, with just the agent and a
goal cell, and of the smallest possible dimensions, 1x1:

==
experiment_world(empty_world).
world_dimensions(1,1).
==

Writing a dataset to file
-------------------------

To write a dataset to a file, call write_dataset/0:

==
?- write_dataset.
==

To open the just-written file in the Swi-Prolog IDE, use
dataset_file_name/2:

==
?- dataset_file_name(_,P), edit(P).
==

The just-written file is then automatically loaded into robots_gen.pl.

Known bugs
----------

Unfortunately, you can't immediately use the generated file- because of
module access complications, an existence error will be raised if you
immediately call a learning predicate on the newly generated file,
particularly if you have just changed the world's dimensions. To avoid
errors, you should start a new Prolog session. We apologies for the
delay this will cause to your journey. Hey, once I got a refund of 2.00
GBP from Southern Rail for a delay of 00:30 min. The system works!

Debugging a dataset
-------------------

Dataset generator debugging facilities are a bit thin on the ground.

You can visually inspect primitive moves with predicates defined in
module render. Ensure that, in render.pl, the option output_to/1 is set
to "console":

==
output_to(console).
==

This will avoid writing to a log file, if one is defined, or raising an
error if one is not. Once output_to/1 is set, you can make the folloqing
query to generate all moves on backtracking, and print them out to
screen in glorious ASCII:

==
?- generate_moves(_Ms), member(M, _Ms), M =.. [_,_Ss,_Gs], render_sequence([_Ss,_Gs]).
# .
x .

. .
# .

M = move_down([0/1, 0/0, 1-1], [0/0, 0/0, 1-1]) ;
# .
. .

x .
# .

M = move_down([0/1, 0/1, 1-1], [0/0, 0/1, 1-1]) ;
# .
. x

. .
# x

M = move_down([0/1, 1/0, 1-1], [0/0, 1/0, 1-1]) .

% etc
==

In the ASCII listing above, the "#" is the navigation agent and the "x"
is the goal. The first grid listed is the start state of a primitive
move action and the second grid is the end state of the same action.
In the example, the listing helps visually check that a "move_down" is
indeed moving the agent one cell down from its starting position.

ASCII symbols for entities and locations in a grid world are listed in
the predicate symbol/3 in render.pl. TODO: make these available at the
configuration level.

Printing a dataset to console
-----------------------------

You can also inspect the results of task and primitive generation by
printing on the console, but be careful of large worlds with a large
number of tasks and moves that will take a very long time to print out.

The following query will generate (and print out in the console) all
tasks, moves and locations in a 2-by-2 world (the dimensions of the
world in the state "vector" are 1-1, but the grid is actually 2x2; it's
an off-by-one error, OK?)

==
?- generate_problems(_Ps), generate_moves(_Ms), generate_locations(_Ps, _Ls), print_clauses(_Ps), nl, length(_Ps,L), length(_Ms, N), print_clauses(_Ms), nl, print_clauses(_Ls), length(_Ls, M).
move([0/0,0/0,1-1],[0/0,0/0,1-1]).
move([0/0,0/1,1-1],[0/1,0/1,1-1]).
move([0/0,1/0,1-1],[1/0,1/0,1-1]).
move([0/0,1/1,1-1],[1/1,1/1,1-1]).
move([0/1,0/0,1-1],[0/0,0/0,1-1]).
move([0/1,0/1,1-1],[0/1,0/1,1-1]).
move([0/1,1/0,1-1],[1/0,1/0,1-1]).
move([0/1,1/1,1-1],[1/1,1/1,1-1]).
move([1/0,0/0,1-1],[0/0,0/0,1-1]).
move([1/0,0/1,1-1],[0/1,0/1,1-1]).
move([1/0,1/0,1-1],[1/0,1/0,1-1]).
move([1/0,1/1,1-1],[1/1,1/1,1-1]).
move([1/1,0/0,1-1],[0/0,0/0,1-1]).
move([1/1,0/1,1-1],[0/1,0/1,1-1]).
move([1/1,1/0,1-1],[1/0,1/0,1-1]).
move([1/1,1/1,1-1],[1/1,1/1,1-1]).

move_down([0/1,0/0,1-1],[0/0,0/0,1-1]).
move_down([0/1,0/1,1-1],[0/0,0/1,1-1]).
move_down([0/1,1/0,1-1],[0/0,1/0,1-1]).
move_down([0/1,1/1,1-1],[0/0,1/1,1-1]).
move_down([1/1,0/0,1-1],[1/0,0/0,1-1]).
move_down([1/1,0/1,1-1],[1/0,0/1,1-1]).
move_down([1/1,1/0,1-1],[1/0,1/0,1-1]).
move_down([1/1,1/1,1-1],[1/0,1/1,1-1]).
move_left([1/0,0/0,1-1],[0/0,0/0,1-1]).
move_left([1/0,0/1,1-1],[0/0,0/1,1-1]).
move_left([1/0,1/0,1-1],[0/0,1/0,1-1]).
move_left([1/0,1/1,1-1],[0/0,1/1,1-1]).
move_left([1/1,0/0,1-1],[0/1,0/0,1-1]).
move_left([1/1,0/1,1-1],[0/1,0/1,1-1]).
move_left([1/1,1/0,1-1],[0/1,1/0,1-1]).
move_left([1/1,1/1,1-1],[0/1,1/1,1-1]).
move_right([0/0,0/0,1-1],[1/0,0/0,1-1]).
move_right([0/0,0/1,1-1],[1/0,0/1,1-1]).
move_right([0/0,1/0,1-1],[1/0,1/0,1-1]).
move_right([0/0,1/1,1-1],[1/0,1/1,1-1]).
move_right([0/1,0/0,1-1],[1/1,0/0,1-1]).
move_right([0/1,0/1,1-1],[1/1,0/1,1-1]).
move_right([0/1,1/0,1-1],[1/1,1/0,1-1]).
move_right([0/1,1/1,1-1],[1/1,1/1,1-1]).
move_up([0/0,0/0,1-1],[0/1,0/0,1-1]).
move_up([0/0,0/1,1-1],[0/1,0/1,1-1]).
move_up([0/0,1/0,1-1],[0/1,1/0,1-1]).
move_up([0/0,1/1,1-1],[0/1,1/1,1-1]).
move_up([1/0,0/0,1-1],[1/1,0/0,1-1]).
move_up([1/0,0/1,1-1],[1/1,0/1,1-1]).
move_up([1/0,1/0,1-1],[1/1,1/0,1-1]).
move_up([1/0,1/1,1-1],[1/1,1/1,1-1]).

end([0/0,0/0,1-1]).
end([0/1,0/1,1-1]).
end([1/0,1/0,1-1]).
end([1/1,1/1,1-1]).
start([0/0,0/0,1-1]).
start([0/0,0/1,1-1]).
start([0/0,1/0,1-1]).
start([0/0,1/1,1-1]).
start([0/1,0/0,1-1]).
start([0/1,0/1,1-1]).
start([0/1,1/0,1-1]).
start([0/1,1/1,1-1]).
start([1/0,0/0,1-1]).
start([1/0,0/1,1-1]).
start([1/0,1/0,1-1]).
start([1/0,1/1,1-1]).
start([1/1,0/0,1-1]).
start([1/1,0/1,1-1]).
start([1/1,1/0,1-1]).
start([1/1,1/1,1-1]).
L = 16,
N = 32,
M = 20.
==

*/

% ========================================
% Dataset writing
% ========================================


%!	write_dataset is det.
%
%	Write a grid navigation dataset to a file.
%
%	The dataset is the set of tasks, moves and locations generated
%	by generate_problems/1, generate_moves/2 and
%	generate_locations/2.
%
%	The dataset is written in a module file exporting the primitive
%	grid world navigation action and primitive location predicates,
%	move_right/2, move_left/2, move_up/2, move_down/2, start/1 and
%	end/1.
%
%	The path for the dataset's module file is generated by
%	dataset_file_name/2.
%
%	Once a dataset file is written it is meant to be imported in
%	robots_gen.pl, and its exported definitions of primitives
%	re-exported to the module user. Predicates in robots_gen.pl find
%	the current file from the path in dataset_file_name/2.
%
%	@tbd
%
write_dataset:-
	dataset_file_name(Bn,Fn)
	,generate_problems(Ps)
	,generate_moves(Ms)
	,generate_locations(Ps,Ls)
	,open(Fn,write,S,[alias(robots_dataset)])
	,Es = [move_right/2,move_left/2,move_up/2,move_down/2,start/1,end/1]
	,format(S,':-module(~w, ~w).~n~n' , [Bn,Es])
	,list_dataset(S,Ps,Ms,Ls)
	,write_atoms(S,Ps)
	,nl(S)
	,write_atoms(S,Ls)
	,nl(S)
	,write_atoms(S,Ms)
	,close(S).


%!	dataset_file_name(-Basename,-Path) is det.
%
%	Generate a Path for a grid world navigation dataset.
%
dataset_file_name(Bn,P):-
	dataset_root_path(R)
	,experiment_world(Wr)
	,world_dimensions(W,H)
	,atomic_list_concat([Wr,W,H],'_',Bn)
	,atom_concat(Bn,'.pl',Fn)
	,atom_concat(R,Fn,P).


%!	list_dataset(+Stream,+Tasks,+Moves,+Locations) is det.
%
%	Print a dataset's properties to a Stream.
%
list_dataset(S,Ps,Ms,Ls):-
	experiment_world(World)
	,world_dimensions(W,H)
	,maplist(length,[Ps,Ms,Ls],[L,M,N])
	,format(S,'% World: ~w~n', [World])
	,format(S,'% Dimensions: ~w x ~w~n', [W,H])
	,format(S,'% Tasks: ~D~n', [L])
	,format(S,'% Primitive Moves: ~D~n', [M])
	,format(S,'% Locations: ~D~n~n', [N]).


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


% ========================================
% Dataset generation
% ========================================


%!	generate_locations(+Moves,-Locations) is det.
%
%	Generate start and end Locations of grid world navigation tasks.
%
%	Moves is a list of primitive moves generated by
%	generate_moves/2. Locations is a list of atoms of start/1
%	and end/1. Each start/1 atom has as its argument the start
%	location of one or more moves in Moves and each end/1 atom has
%	as its argument the end location of one or more moves in Moves.
%
generate_locations(Ms,Ls):-
	setof(S
	     ,M^Ms^F^Ss^Gs^SS^(member(M,Ms)
			      ,M =.. [F,Ss,Gs]
			      ,SS = [start(Ss),end(Gs)]
			      ,member(S,SS)
			      )
	     ,Ls).


%!	generate_moves(+Moves) is det.
%
%	Generate extensional move/2 definitions.
%
%	Generates all possible moves in the current grid world.
%
%	Moves is a list of atoms of the predicates move_right/2,
%	move_left/2, move_up/2 and move_down/2, representing all
%	primitive moves that it is possible to make in the current grid
%	world.
%
%	atoms in Moves are generated by taking each start state
%	generated by world:problem/6 and calling move/3 with it as
%	input.
%
generate_moves(Ms):-
	experiment_world(Wr)
	,world_dimensions(W,H)
	,findall(M
		,(problem(Wr,nondeterministic,W,H,Ss,_)
		 ,move(Ss,_,M)
		 )
		,Ms_)
	,sort(Ms_, Ms).


%!	move(+Start,+End,-Move) is det.
%
%	Generate a ground atom representing a grid world move.
%
%	Start and End are starting and end locations of a robot
%	navigation task, generated by problem/6.
%
%	Move is a ground atom of one of the predicates move_right/2,
%	move_left/2, move_up/2 or move_down/2 that is true for the given
%	Start and End locations.
%
move(Ss,Gs,move_right(Ss,Gs)):-
	move_right(Ss,Gs).
move(Ss,Gs,move_left(Ss,Gs)):-
	move_left(Ss,Gs).
move(Ss,Gs,move_up(Ss,Gs)):-
	move_up(Ss,Gs).
move(Ss,Gs,move_down(Ss,Gs)):-
	move_down(Ss,Gs).


%!	generate_problems(+Problems) is det.
%
%	Generate a list of grid world navigation Problems.
%
%	Each problem in Problems is an atom of the predicate move/2
%	where the first atom is the starting state of a grid world
%	navigation task and the second argument is the end state of that
%	task (when the taks is solved successfully).
%
%	The grid world setup and dimensions are defined in
%	experiment_world/1 and world_dimensions/2. Problems are
%	generated by calling problem/6 and passing it the arguments of
%	the two predicates above and "nondeterministic" as the second
%	argument (controlling the method of problem generation).
%	"nondeterministic" generation means that all possible navigation
%	tasks in the given world are generated on backtracking.
%
generate_problems(Ps):-
	experiment_world(Wr)
	,world_dimensions(W,H)
	,findall(move(Ss,Gs)
		,problem(Wr,nondeterministic,W,H,Ss,Gs)
		,Ps).



%!	move_right(+State,-New) is det.
%
%	Move the robot to the right, with or without the ball.
%
move_right(Ss,_Gs):-
	\+ ground(Ss)
	,!
	,fail.
move_right([R,G,W-H],[R_new,G,W-H]):-
% Empty world
	!
	,move(R,+,1/0,W-H,R_new).


%!	move_left(+State, -New) is det.
%
%	Move to the left.
%
move_left(Ss,_Gs):-
	\+ ground(Ss)
	,!
	,fail.
move_left([R,G,W-H],[R_new,G,W-H]):-
	!
	,move(R,-,1/0,W-H,R_new).


%!	move_up(+State, -New) is det.
%
%	Move on up.
%
move_up(Ss,_Gs):-
	\+ ground(Ss)
	,!
	,fail.
move_up([R,G,W-H],[R_new,G,W-H]):-
	!
	,move(R,+,0/1,W-H,R_new).


%!	move_down(+State, -New) is det.
%
%	Move on down.
%
move_down(Ss,_Gs):-
	\+ ground(Ss)
	,!
	,fail.
move_down([R,G,W-H],[R_new,G,W-H]):-
	!
	,move(R,-,0/1,W-H,R_new).


%!	move(+Point,+Delta,+Distance,+Limits,-End) is det.
%
%	Modify a coordinate, respecting spatial Limits.
%
%	Point is a compound X/Y wher X,Y are numbers, representing a
%	coordinate. Delta is one of [-,+], signifying how Point is to be
%	modified. Distance is a compound, Dx/Dy, where Dx,Dy are
%	numbers, each the amount by which the corresponding value in
%	Point is to be modified according to Delta. Limits is a
%	key-value pair, W-H, where each of W, H are numbers, the upper
%	limit of the two dimensions of the current world.
%
%	move/5 is true when End = Point Delta Distance and End_X in
%	[0,W], End_Y in [0,H].
%
move(X/Y,D,Dx/Dy,W-H,Ex/Ey):-
	ground(X/Y)
	,ground(Dx/Dy)
	,ground(D)
	,ground(W-H)
	,Mv_x =.. [D,X,Dx]
	,Mv_y =.. [D,Y,Dy]
	,Ex is Mv_x
	,Ey is Mv_y
	,within_limits(Ex,W)
	,within_limits(Ey,H).


%!	within_limits(+Distance,+Limit) is det.
%
%	True when a moving Distance is within the given Limit.
%
within_limits(X,L):-
	integer(X)
	,integer(L)
	,X >= 0
	,X =< L.