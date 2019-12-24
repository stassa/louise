:-module(render, [render_problem/1
		 ,render_sequence/1
		 ,render_world/1
		 ,output/2
		 ,output_nl/0
		 ]).

:-use_module(generator_configuration).

/** <module> World rendering

Predicates for presenting the state of a grid world visually in a human
friendly representation.

Representing the world
======================

Predicates in this file are designed to be representation- agnostic, up
to a certain extent (not a big extent). They expect the world to be a
grid world represented as a state-vector (well, a list really) of the
form:

[Coords_1,...,Coords_n|[Extras|Limits]]

Where Coords_1 ... Coords_n are the coordinates of each object given as
X/Y compounds, listing the X and Y coordinate of the object; Extras is
any additional information required to determine the state of the world;
and Limits are the extents of the X and Y dimensions of the world.

For the time being, the additional information in Extras includes a
boolean listing whether the robot is holding the ball. Different
problems may hold different information.


Rendering the world
===================

World rendering proceeds in two steps: first, the world state-vector is
used to build a visual reprsentation of the world; then this visual
representation is rendered to screen, for the user's delectation.

For the time being, the world is rendered in a text-based manner, by
printing it out to standard output as a grid of alphanumeric characters.

Each object of interest, whose coordinates are given in Coords_1, ...,
Coords_n, (e.g. the robot and the ball in the robot-ball-goal-X worlds)
is rendered as a different character, defined by the object_symbol/3 and
symbol/3 predicates. The same is true for empty space in the world,
where none of the objects of interest are currently situated.

Each world may include its own set of objects of interest. This means
that, with each world that must be rendered by predicates in this file,
a new set of object_symbol/3 and symbol/3 clauses must be provided.

Currently, the way to do this is to add the new clauses needed in this
file, which is really the most simple way to extend the rendering
functionality herewith. There are slightly more complicated
alternatives that may become more necessary in the future, as more and
more diverse worlds are required to be rendered. In particular, it
should be possible to define each new world as a separate file
(including world generation and actions, alongside rendering predicates)
and declare the predicates required to be extended as multifile. For the
time being, this is not particularly necessary so it's not implemented
yet.

*/


%!	render_problem(+Problem) is det.
%
%	Render the start and goal states of a Problem.
%
%	Problem is a problem atom, such as move(Ss,Gs) or plan(Ss,Gs)
%	etc, where Ss is the start state and Gs the end state of a
%	planning problem.
%
render_problem(P):-
	P =.. [_,Ss,Gs]
	,output('Starting state: ~w~n',[Ss])
	,render_world(Ss)
	,output('~nGoal state: ~w~n',[Gs])
	,render_world(Gs)
	,output_nl.



%!	render_sequence(+States) is det.
%
%	Render a sequence of States, possibly a path to a goal state.
%
render_sequence(Ss):-
	forall(member(S, Ss)
	      ,(render_world(S)
	       ,output_nl
	       %,nl
	       )
	      ).


%!	render_world(+State) is det.
%
%	Render the current State of the world.
%
%	Pretty-prints the world State-vector, for human eyes and minds.
%
render_world(Ss):-
	member(W-H, Ss)
	% An object's goal state may be unknown.
	% e.g. the opponent's state in rugby_world.
	,ground(W-H)
	,build_world(Ss,Bs)
	,present_world(W,H,Bs).


%!	build_world(+State,-Symbols) is det.
%
%	Convert State to a list of Symbols for printing.
%
%	The State vector is translated to a list of Symbols reprsenting
%	the world, starting from the top-left corner of the world-grid,
%	filling in each row column-wise, then moving to the next row
%	down, until the first.
%
%	The symbol added at each coordinate is determined by the
%	predicates object_symbol/n and symbol/3. object_symbol/n is
%	world-specific (hence the variable arity, "n"). symbol/3 is
%	common for each world.
%
%	@tbd This could very well just print the world in one go,
%	instead of building it up first- but, we may need the world-list
%	for further processing. Probably not, since we got the world
%	State, but, who knows. Actually- I know. We may want to render
%	the world in a different manner, e.g. using images etc.
%
build_world(Ss,Bs):-
	member(W-H,Ss)
	,ground(W-H)
	,findall(S
		% Decrement rows
	       ,(between(0,H,I)
		,Y is H - I
		% Increment columns.
		,between(0,W,X)
		,once(object_symbol(X/Y,Ss,S))
		)
	       ,Bs).


%!	object_symbol(+Point,+World,-Symbol) is det.
%
%	A Symbol representing an object occupying a Point in the World.
%
%	True when Point matches the coordinates, X/Y, of an object in
%	the World. Then, Symbol is bound to the character used to
%	represent that object when rendering the world.
%
/* Empty world */
object_symbol(R,[R,_G,_],S):-
	symbol(robot,false,S).
object_symbol(XY,[_R,G,_],S):-
% If the state comes from a primitive move the goal will be a free
% variable. See generate_moves/1 and closure in move_generator.pl
	XY == G
	,symbol(goal,false,S).
object_symbol(_,[_,_,_],S):-
	symbol(floor,false,S).

/* Simple world */
% This and rest of worlds need to deal with goals left unbound from
% primitive move generator.
object_symbol(R,[R,_B,_G,HB,_],S):-
	symbol(robot,HB,S).
object_symbol(B,[_R,B,_G,HB,_],S):-
	symbol(ball,HB,S).
object_symbol(G,[_R,_B,G,HB,_],S):-
	symbol(goal,HB,S).
object_symbol(_,[_,_,_,HB,_],S):-
	symbol(floor,HB,S).

/* Obstacles world */
object_symbol(R,[R,_,_,_,HB,_],S):-
	symbol(robot,HB,S).
object_symbol(O,[_,_,O,_,HB,_],S):-
	symbol(obstacle,HB,S).
object_symbol(B,[_,B,_,_,HB,_],S):-
	symbol(ball,HB,S).
object_symbol(G,[_,_,_,G,HB,_],S):-
	symbol(goal,HB,S).
object_symbol(_,[_,_,_,_,HB,_],S):-
	symbol(floor,HB,S).

/*  Rugby World  */
object_symbol(XY,[rugby_world,R,B,O,G,HB,WH],S):-
% Variable opponent position - goal state.
	\+ ground(O)
	,!
	,object_symbol(XY,[R,B,G,HB,WH],S).
object_symbol(XY,[rugby_world,R,B,O,G,HB,WH],S):-
% Same as obstacles world, other than the world label.
	object_symbol(XY,[R,B,O,G,HB,WH],S).


%!	present_world(+Width,+Height,+World) is det.
%
%	Pretty-print the current state of the World
%
%	World is a world-build, as constructed by build_world/2. This
%	prints the world as a matrix of symbols, including the ball, the
%	robot and the floor tiles.
%
%	@tbd As hinted at in build_world/2, this is just one way to
%	present the world. We might want to use some other presentation
%	in the future, e.g. images etc.
%
present_world(W,H,Ws):-
	present_cols(0,0,W,H,Ws).


%!	present_cols(+X,+Y,+Width,+Height,+World) is det.
%
%	Business end of present_world/4.
%
%	Proceeds column-by-column, printing one row in each step.
%
present_cols(_,Y,_,H,[]):-
	Y > H
	,!.
present_cols(X,Y,W,H,Ss):-
	present_row(X,W,Ss,Ss_)
	,output_nl
	,succ(Y,Y_)
	,present_cols(X,Y_,W,H,Ss_).


%!	present_row(+X,+Width,+World,-Rest) is det.
%
%	Business end of present_cols/5. Proceeds row-wise, printing one
%	symbol in each step.
%
present_row(W,W,[S|Ss],Ss):-
	output('~w ',[S])
	,!.
present_row(X,W,[S|Ss],Bind):-
	output('~w ',[S])
	,succ(X,X_)
	,present_row(X_,W,Ss,Bind).


%!	output(+Format,+String) is det.
%
%	Print a debug String with the given Format atom.
%
%	@tbd This is here mostly because I also had it in the rendering
%	module for the original robots experiment dataset (as
%	debug_format/2). In that module it would take care of logging to
%	a dedicated log file etc. In this set of experiments we don't
%	(yet) have such complicated logging, so it just logs to the
%	current log output for robots, which is simply defined with a
%	directive at the start of the robot experiment file (a directive
%	like :-debug(robots>'robot_experiment.log') or such).
%
output(F,Ss):-
	output_to(log)
	,!
	,format(robots,F,Ss).
output(F,Ss):-
	output_to(console)
	,format(F,Ss).


%!	output_nl is det.
%
%	Print a new line to the current debug stream for robots.
%
%	@tbd This too is a little redundant currenlty. See comments in
%	output/2.
%
output_nl:-
	output_to(log)
	,!
	,format(robots,'~n',[]).
output_nl:-
	output_to(console)
	,nl.
