:-module(world, [problem/6
		,object_location/4
		%,nondeterministic_problem/5
		%,fixed_problem/5
		%,random_problem/5
		]).

/** <module> Problem generator

Generator for grid worlds of a random width and height with a robot,
ball, goal and possibly other objects of interest placed at random
coordinates within, and the goal state for each such world.

*/


%!	problem(+World,+Generation,+Width,+Height,+Starting,+Goal) is
%!	nondet.
%
%	Generate a problem grid for a robot world.
%
%	World is one of the known worlds, defined by sets of actions in
%	module actions.
%
%	Generation determines the method by which problems will be
%	generated, including whether grid dimensions are fixed to the
%	given Width and Height, or allowed to range between 2 and those
%	values at random, and whether a single problem grid will be
%	generated or multiples. Generation is one of the following:%
%	* nondeterministic: All possible problem grids with the given
%	Width and Height are generated on backtracking.
%	* incremental: As "nondeterminisic" but the dimensions of the
%	generated problem grids range in the intervals [1,Width] and
%	[1,Height] respectively. Note that this means that each possible
%	combination of Width and Height values is generated on
%	backtracking.
%	* square: As "incremental" but Width and Height are only allowed
%	to be equal, generating only square problem grids. Fails if
%	Width \= Height!
%	* infinite: Generates an unrestricted number of grid worlds with
%	random dimensions between 2 and Width/Height. Be careful with
%	this option: it will "go infinite" if called within a
%	failure-driven loop. Also, there is no attempt to avoid
%	duplicates (think of it as sampling with replacement).
%	* fixed: Generates a single problem grid with randomly chosen
%	dimensions exactly equal to the given Width and Height. When
%	Generation is "fixed", problem/6 behaves deterministically.
%	* random: Generates a single problem grid with dimensions as
%	randomly chosen numbers between 2 and Width/Height. When
%	Generationn is "random", problem/6 will behave
%	nondeterministically.
%
problem(World,nondeterministic,W,H,Ss,Gs):-
	nondeterministic_world(World,W,H,Ss)
	,once(goal_state(World,Ss,Gs)).
problem(World,incremental,W,H,Ss,Gs):-
	between(1,W,W_)
	,between(1,H,H_)
	,nondeterministic_world(World,W_,H_,Ss)
	,once(goal_state(World,Ss,Gs)).
problem(World,square,WH,WH,Ss,Gs):-
	between(1,WH,WH_)
	,nondeterministic_world(World,WH_,WH_,Ss)
	,once(goal_state(World,Ss,Gs)).
problem(World,infinite,W,H,Ss,Gs):-
	between(1,inf,_)
	,random_world(World,W,H,Ss)
	,once(goal_state(World,Ss,Gs)).
problem(World,fixed,W,H,Ss,Gs):-
	!
	,fixed_problem(World,W,H,Ss,Gs).
problem(World,random,W,H,Ss,Gs):-
	random_problem(World,W,H,Ss,Gs).



%!	fixed_problem(+Width,+Height,+Start,+Goal) is det.
%
%	Generate a random problem for a world of fixed Width and Height.
%
%	As random_problem, but with fixed world dimensions.
%
fixed_problem(World,W,H,Ss,Gs):-
	once(random_world(World,W,H,Ss))
	,once(goal_state(World,Ss,Gs))
	,!.
fixed_problem(World,W,H,Ss,Gs):-
	fixed_problem(World,W,H,Ss,Gs).


%!	random_problem(+Max_Width,+Max_Height,+Start,-Goal) is det.
%
%	Generate a random problem for a world of the given dimensions.
%
%	Max_Width and Max_Height are the maxima of the x and y
%	dimensions, respectively, each of which starts at 0, making up
%	a system of cartesian coordinates in two dimensions.
%
%	Start and Goal are the initial and goal states of a problem in
%	the generated world. The initial state is selected by placing
%	the robot, ball and goal at random coordinates in the generated
%	world. The goal state is generated nondeterministically from the
%	initial state.
%
random_problem(World,W_max,H_max,Ss,Gs):-
	once(random_dimensions(World,W_max,H_max,W,H))
	,once(random_world(World,W,H,Ss))
	,once(goal_state(World,Ss,Gs))
	,!.
random_problem(World,W_max,H_max,Ss,Gs):-
% If generating a problem failed, try again.
% A problem will normally fail when objects
% cannot be placed in a valid configuration,
% e.g. when the goal is on an obstacle in the
% obstacles world, etc.
	random_problem(World,W_max,H_max,Ss,Gs).


%!	random_dimensions(+Max_Width,+Max_Height,-Width,-Height) is det.
%
%	Randomly select the dimensions of a problem world.
%
%	Width and Height are selected as random numbers between 2 and
%	Max_Width and Max_Height, respectively. In other words, the
%	world is never allowed to have fewer than 4 floor tiles in
%	total, so as to always be able to accommodate the three entities
%	in the problem.
%
random_dimensions(World,W_max,H_max,W,H):-
	memberchk(World,[empty_world,simple_world])
	,random_between(1,W_max,W)
	,random_between(1,H_max,H).
random_dimensions(obstacles_world,W_max,H_max,W,H):-
	must_be(between(2,inf), W_max)
	,must_be(between(2,inf), H_max)
	,random_between(2,W_max,W)
	,random_between(2,H_max,H).
random_dimensions(rugby_world,W_max,H_max,W,H):-
% Will have to give more specific dimensions for Rugby world.
	must_be(between(2,inf), W_max)
	,must_be(between(2,inf), H_max)
	,random_between(2,W_max,W)
	,random_between(2,H_max,H).


%!	goal_sate(+World,-Goal) is det.
%
%	Concstruct the Goal state for a World.
%
%	The goal state always has the same structure: the robot and the
%	ball must be on the goal tile and the robot must not be holding
%	the ball.
%
goal_state(empty_world,[_R,G,W-H],[G,G,W-H]).
goal_state(simple_world,[_R,_B,G,_HB,W-H],[G,G,G,false,W-H]).
goal_state(obstacles_world,[_R,_B,O,G,_HB,W-H],[G,G,O,G,false,W-H]).
% In rugby world, the opponent's end state is unknown (i.e. left
% unbound).
goal_state(rugby_world,[rugby_world,_R,_B,_O,G,_HB,W-H],[rugby_world,G,G,_,G,false,W-H]).


%!	nondeterministic_world(+World,+Width,+Height,-Starting_State) is
%!	det.
%
%	Generate a nondeterministic robot-ball-goal world.
%
%	All possible valid worlds are generated on backtracking.
%
nondeterministic_world(empty_world,W,H,[R,G,W-H]):-
	object_coordinates(nondet,W,H,R)
	,object_coordinates(nondet,W,H,G).
nondeterministic_world(simple_world,W,H,[R,B,G,HB,W-H]):-
	object_coordinates(nondet,W,H,R)
	,object_coordinates(nondet,W,H,B)
	,object_coordinates(nondet,W,H,G)
	,holds_ball(nondet,R,B,HB).
nondeterministic_world(obstacles_world,W,H,[R,B,O,G,HB,W-H]):-
	object_coordinates(nondet,W,H,R)
	,object_coordinates(nondet,W,H,B)
	,object_coordinates(nondet,W,H,G)
	,object_coordinates(nondet,W,H,O)
	% The goal can't be on the obstacle
	% or the robot will never get there.
	,G \= O
	% Neither can the ball
	,B \= O
	,holds_ball(nondet,R,B,HB).


%!	random_world(+Width,+Height,-World) is det.
%
%	Generate a random robot-ball-goal world.
%
random_world(empty_world,W,H,[R,G,W-H]):-
	object_coordinates(rand,W,H,R)
	,object_coordinates(rand,W,H,G).
random_world(simple_world,W,H,[R,B,G,HB,W-H]):-
	object_coordinates(rand,W,H,R)
	,object_coordinates(rand,W,H,B)
	,object_coordinates(rand,W,H,G)
	,holds_ball(rand,R,B,HB).
random_world(obstacles_world,W,H,[R,B,O,G,HB,W-H]):-
	object_coordinates(rand,W,H,R)
	,object_coordinates(rand,W,H,B)
	,object_coordinates(rand,W,H,G)
	,object_coordinates(rand,W,H,O)
	% The goal can't be on the obstacle
	% or the robot will never get there.
	,G \= O
	% Neither can the ball
	,B \= O
	,holds_ball(rand,R,B,HB).
random_world(rugby_world,W,H,[rugby_world,R,B,O,G,HB,W-H]):-
	object_coordinates(rand,W,H,R)
	,object_coordinates(rand,W,H,B)
	,object_coordinates(rand,W,H,G)
	,object_coordinates(rand,W,H,O)
	% The opponent and the robot can't coincide
	% or they will keep moving together.
	,O \= R
	% The goal can't be on the obstacle
	% or the robot will never get there.
	,G \= O
	% Neither can the ball
	,B \= O
	,holds_ball(rand,R,B,HB).


%!	object_coordinates(+Kind,+Width,+Height,-Coordinates) is det.
%
%       Select a set of world Coordinates for an object.
%
%	Kind is one of [rand,det], denoting whether the coordinates will
%	be selected at random, or nondeterministically producing each pair
%	of X and Y coordinates within the limits set by Width and Height
%	on successive backtracking, respectively.
%
object_coordinates(rand,W,H,X/Y):-
	!
	,random_between(0,W,X)
	,random_between(0,H,Y).
object_coordinates(nondet,W,H,X/Y):-
	between(0,W,X)
	,between(0,H,Y).


%!	holds_ball(+Kind,+Robot,+Ball,-Holds) is det.
%
%	Decide whether the Robot is holding the Ball, at random.
%
%	Kind is one of [rand,det], denoting whether Holds will be
%	generated at random, or nondeterministically, producing each
%	value on backtracking.
%
holds_ball(rand,R,R,HB):-
	!
	,random_member(HB, [true,false]).
holds_ball(nondet,R,R,HB):-
	member(HB,[true,false]).
holds_ball(_,R,B,false):-
	R \= B.


%!	object_location(+World,+State,+Object,-Coordinates) is semidet.
%
%	Determine the Coordinates where an Object is currently located.
%
%	The following objects can be located in each known world:
%	* empty_world: [robot,goal]
%	* simple_world: [robot,ball,goal]
%	* obstacles_world: [robot,ball,obstacle,goal]
%	* rugby_world: [robot,ball,opponent,goal]
%
object_location(empty_world,[R,_,_],robot,R).
object_location(empty_world,[_,G,_],goal,G).
object_location(simple_world,[R,_B,_G,_,_],robot,R).
object_location(simple_world,[_R,B,_G,_,_],ball,B).
object_location(simple_world,[_R,_B,G,_,_],goal,G).
object_location(obstacles_world,[R,_B,_O,_G,_,_],robot,R).
object_location(obstacles_world,[_R,B,_O,_G,_,_],ball,B).
object_location(obstacles_world,[_R,_B,O,_G,_,_],obstacle,O).
object_location(obstacles_world,[_R,_B,_O,G,_,_],goal,G).
object_location(rugby_world,[rugby_world,R,_B,_O,_G,_,_],robot,R).
object_location(rugby_world,[rugby_world,_R,B,_O,_G,_,_],ball,B).
object_location(rugby_world,[rugby_world,_R,_B,O,_G,_,_],opponent,O).
object_location(rugby_world,[rugby_world,_R,_B,_O,G,_,_],goal,G).
