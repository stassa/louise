:-module(robots, [background_knowledge/2
		 ,metarules/2
		 ,positive_example/2
		 ,negative_example/2
		 ,learn_and_render_plan/2
		 ,render_plan/3
		 ,double_move/3
		 ,triple_move/3
		 ,quadruple_move/3
		 ,move/1
		 ,move_right_twice/2
		 ,move_left_twice/2
		 ,move_up_twice/2
		 ,move_down_twice/2
		 ,move_right_then_up/2
		 ,move_right_then_down/2
		 ,move_left_then_up/2
		 ,move_left_then_down/2
		 ,move_up_then_right/2
		 ,move_up_then_left/2
		 ,move_down_then_right/2
		 ,move_down_then_left/2
		 ]).


:-use_module(configuration).
:-use_module(src(auxiliaries)).
:-use_module(render).
:-use_module(world).

% Also loads generator configuration, world.pl and render.pl
:- user:use_module(move_generator).
% Stops Swi from raising exception on move/2. No idea why.
:- dataset_file_name(_,Fn)
  ,user:use_module(Fn).


/* % Uncomment to work on a new dataset or debuggin of tasks.
:-edit(world).
:-edit(render).
:-edit(move_generator).
:-edit(generator_configuration).
*/

/** <module> Grid worl navigation.

Generates grid worlds with various elements as a learning environment
for navigation strategies.

Full documentation pending. Note that training with the full dataset can
take a very long time. Try this kind of query instead:

==
?- train_and_test(move/2,1,_Ps,acc,V), print_clauses(_Ps).
==

Predictive accuracy should be very low, since a single example is
selected, but learning will be quick and you'll have a chance to inspect
the results. Training with multiple examples tends to generate _very_
large programs! That's because each training example is a navigation
task with multiple solutions (multiple paths between each two points A
and B in the grid world, unless of course A = B) and each sub-hypothesis
is a single clause. As the problem grows, so does the number of
"alternatives", so this is even more evident in larger worlds. Training
on a 4x4 grid with a 0.9 sampling rate can generate programs of more
than 2k clauses. If you do that it's a good idea to not print the
results in the console. You've been warned!

*/

% ========================================
% MIL problem
% ========================================

% Triadic metarules - allow higher-order moves to be used.
configuration:tri_chain_1 metarule 'P(x,y):- Q(M,x,z), R(z,y)'.
configuration:tri_chain_2 metarule 'P(x,y):- Q(x,z), R(M,z,y)'.
configuration:tri_chain_3 metarule 'P(x,y):- Q(M,x,z), R(N,z,y)'.

background_knowledge(move/2, [% Move primitives
			      move_right/2
			     ,move_left/2
			     ,move_up/2
			     ,move_down/2
			     % Uncomment when training in worlds where the
			     % agent must manipulate an object, i.e.
			     % simple_world and obstacles_world
			     % TODO: make this addition automatic, maybe?
			     %,pick_up/2
			     %,put_down/2
			      % Higher-order actions - multi-moves
			     ,double_move/3
			     ,triple_move/3
			     ,quadruple_move/3
			      % Compound actions - double moves
			     ,move_right_twice/2
			     ,move_left_twice/2
			     ,move_up_twice/2
			     ,move_down_twice/2
			      % Compound actions - angled moves
			     ,move_right_then_up/2
			     ,move_right_then_down/2
			     ,move_left_then_up/2
			     ,move_left_then_down/2
			     ,move_up_then_right/2
			     ,move_up_then_left/2
			     ,move_down_then_right/2
			     ,move_down_then_left/2
			  ]).

metarules(move/2,[chain
		 ,tri_chain_1
		 ,tri_chain_2
		 ,tri_chain_3
		 %,projection_21
		 %,projection_21_abduce
		 %,chain_abduce_x
		 %,chain_abduce_y
		 %,chain_abduce_z
		 %,precon_abduce
		 %,postcon_abduce
		 ]).

positive_example(move/2,move(Ss,Gs)):-
	dataset_file_name(Bn,_)
	,Bn:move(Ss,Gs).

negative_example(move/2,_):-
	fail.


% ========================================
% Hypothesis rendering
% ========================================

%!	higher_order_moves(?Moves) is semidet.
%
%	A list of higher order Moves.
%
higher_order_moves([double_move/3
		   ,triple_move/3
		   ,quadruple_move/3
		   ]).


%!	learn_and_render_plan(+Target,+Sample) is det.
%
%	Sample k examples for Target and trace a learned plan.
%
learn_and_render_plan(T,S):-
	experiment_data(T,Pos,Neg,BK,MS)
	,k_list_partitions(S,Pos,Es,_)
	,learn(Es,Neg,BK,MS,Ps)
	,forall(member(E,Es)
	       ,render_plan(Ps,E,BK)
	       ).


%!	render_plan(+Plan,+Example,+BK) is nondet.
%
%	Render the steps of a Plan.
%
%	Plan is the learned hypothesis, a grid world navigation plan,
%	Example is a positive example of move/2 and BK are the symbols
%	and arities of the background knowledge used to learn Plan.
%
%	render_plan/3 executes the Plan for the given Example and
%	renders each step of the Plan's execution. Rendering uses the
%	predicates defined in render.pl. The default and currently only
%	rendering mode is glorious ASCII. Other modes may be defined in
%	the future.
%
%	@bug If a solution is left-recursive, render_plan/3 will get
%	stuck in an infinite recursion.
%
render_plan(Ps,E,BK):-
% Copy term to ignore variable bindings when printing.
	copy_term(Ps, Ps_)
	,closure(BK,user,Cs)
	,flatten([Ps_,Cs], Ts)
	,output('Navigation Task:~n',[])
	,render_problem(E)
	,print_clauses('Tracing plan:',Ps)
	,output_nl
	,prove_list(E,Ts,[],Ms_)
	,reverse(Ms_,Ms)
	,render_moves_sequence(Ms)
	,!.
render_plan(_Ps,_E,_BK):-
	output('Failed to render plan',[]).

%!	prove_list(+Literals,+Program,+Acc,-Trace) is nondet.
%
%	Prove a Program given as a list and Trace its proof.
%
%	Meta-interpreter that takes a Program as a list of clauses and
%	returns ish a list representing a Trace of the Program's proof,
%	again as a list of clauses.
%
%	Used to construct a trace of proving a learned hypothesis
%	representing a grid navigation plan so that it can later be
%	rendered.
%
prove_list(true,_Ps,Ss,Ss).
prove_list((L1),_Ps,Acc,[L1|Acc]):-
% If L1 is a higher-order move atom hand over to call/1.
	higher_order_move(L1)
	,call(L1).
prove_list((L1,Ls),Ps,Acc,Bind):-
	prove_list(L1,Ps,Acc,Acc1)
	,prove_list(Ls,Ps,Acc1,Bind).
prove_list((L1),Ps,Acc,Bind):-
	program_clause(L1,Ps,B)
	,prove_list(B,Ps,[L1|Acc],Bind).


%!	higher_order_move(+Move) is nondet.
%
%	True when Move is an atom of a higher-order move.
%
%	It is convenient to interpret higher order moves differently
%	than primitive and composite moves- because higher order moves
%	are defined in terms of meta-calls, i.e. with call/[2,3,4] etc.
%	Meta-calls are not easily interpretable by means of a
%	meta-interpretable, so once one is identified we just hand over
%	to the Prolog interpreter and let it do its thing. This
%	predicate takes care of the identification part of that process.
%
higher_order_move(L):-
	higher_order_moves(Ms)
	,functor(L,F,A)
	,member(F/A,Ms).


%!	program_clause(+Literal,+Program,-Body) is det.
%
%	Find the Body of a clause matching Literal in Program.
%
%	Replaces the call to clause/2 used to find a resolvent in a
%	program stored in dynamic memory, in a typical meta-interpreter.
%
program_clause(L,Ps,B):-
	member(L:-B,Ps).
program_clause(L,Ps,true):-
	true_atom(L)
	,member(L,Ps).


%!	true_atom(+Literal) is det.
%
%	True when Literal is a true atom.
%
%	An atom P(T1,T2,...,Tn) is true if it's a ground unit clause.
%
%	So, normally, unit clauses would be stored in the dynamic
%	database as definite clauses with the atom "true" as the only
%	body literal. However, in prove_list/2 we expect the program to
%	be a list of program clauses, including unit clauses _without_
%	any body literals at all, we need a predicate that can tell when
%	a literal is a unit clause. This is it.
%
true_atom(L1):-
	L1 =.. [F|_As]
	,F \= (:-).


%!	render_moves_sequence(+Moves) is det.
%
%	Render a set of Moves as a sequence of grid world states.
%
%	Used to print the plan trace constructed by prove_list/4 as
%	a sequence of grid-world state-lists, represnting a path
%	from a starting state to an end state.
%
render_moves_sequence([M|Ms]):-
	move_states(M,Ss,_Gs)
	,bind_goal(M,Ms)
	,render_world(Ss)
	,output('~w~n',[M])
	,output_nl
	,render_moves_sequence(Ms,Ss).


%!	move_states(+Move,-Starting_State,-End_State) is det.
%
%	Extract the starting and end states of a Move atom.
%
%	Slim interface to dyadic and triadic moves, used to extract
%	their starting state to seed the moves_sequence/3
%	state-accumulator.
%
move_states(M,Ss,Gs):-
% Primitive and composite moves.
	M =.. [_,Ss,Gs]
	,!.
move_states(M,Ss,Gs):-
% Higher-order move - second arg is a move symbol.
	M =.. [_,_,Ss,Gs].


%!	bind_goal(+Move,+Moves) is det.
%
%	Ensure goals in Moves are bound.
%
%	Move is a ground move/2 atom, a grid world navigation task.
%	Moves is a sequence of moves representing a trace of a learned
%	plan, as constructed by prove_list/4. The goal location in each
%	move in Moves is unbound so rendering one of a move's states
%	will not render the goal. This predicate ensures that the
%	position of the goal in each move in Moves is bound to the goal
%	location in Move, which, as a ground task, does have a location
%	for the goal.
%
bind_goal(move(Ss,_Gs),Ms):-
	object_location(empty_world,Ss,goal,G)
	,term_variables(Ms,Vs)
	,findall(G
		,member(G,Vs)
		,Vs).


%!	render_moves_sequence(+Moves,+Acc) is nondet.
%
%	Business end of render_moves_sequence/1.
%
%	Acc is the accumulator of sates. It's actually not a list- since
%	we don't need to return anything at the end of processing. It
%	just stores the last state that was rendered so that we can
%	follow a path from start to end state, while skipping duplicate
%	states (which is to say, states of sub-moves, like primitive
%	moves taken in the process of taking composite moves. These
%	are added to the trace by prove_list/4 but ignored for the
%	purpose of rendering by this predicate).
%
render_moves_sequence([],_Ss).
render_moves_sequence([M|Ms],Ss):-
	move_states(M,Ss,Gs)
	,render_world(Gs)
	,output('~w~n',[M])
	,output_nl
	,!
	,render_moves_sequence(Ms,Gs).
render_moves_sequence([_M|Ms],Acc):-
% _M is a primitive or composite move that has to be skipped because its
% parent move was already rendered and the parent subsumes _M.
	render_moves_sequence(Ms,Acc).



% ========================================
% Background knowledge definitions (local)
% ========================================

% ========================================
% Higher-order actions - multi-moves


%!	double_move(+Move,+Start,-End) is det.
%
%	Repeat a single Move twice.
%
%	Move is the predicate symbol of a primitive or compound move.
%	The allowed moves are listed in move/1.
%
%	@tbd This and the other two higher-order actions are very
%	simple, repeating the same move two or more times. It is
%	certainly possible to generalise this ability to multi-moves of
%	arbitrary cardinality and combining different sub-moves, or
%	combining higher-order moves. However, this would start to get
%	complicated quickly, whereas the current setup is simple enough
%	to implement and explain.
%
double_move(M,Ss,Gs):-
	move(M)
	,call(M,Ss,Ss_1)
	,call(M,Ss_1,Gs).


%!	triple_move(+Move,+Start,-End) is det.
%
%	Repeat a single Move three times.
%
triple_move(M,Ss,Gs):-
	move(M)
	,call(M,Ss,Ss_1)
	,double_move(M,Ss_1,Gs).


%!	quadruple_move(+Move,+Start,-End) is det.
%
%	Repeat a single Move four times.
%
quadruple_move(M,Ss,Gs):-
	move(M)
	,double_move(M,Ss,Ss_1)
	,double_move(M,Ss_1,Gs).


%!	move(?Move) is nondet.
%
%	A Move that can be repeated as a higher-order action.
%
%	Note that this includes already twice-repeating actions, like
%	move_right_twice and move_up_twice. This may or may not be as
%	required. If not, uncomment the actions you don't want, below.
%
move(experiment_file:M):-
% We must point to where M is defined or errors rise.
	member(M, [move_right
		  ,move_left
		  ,move_up
		  ,move_down
		   % Compound actions - double moves
		  ,move_right_twice
		  ,move_left_twice
		  ,move_up_twice
		  ,move_down_twice
		   % Compound actions - angled moves
		  ,move_right_then_up
		  ,move_right_then_down
		  ,move_left_then_up
		  ,move_left_then_down
		  ,move_up_then_right
		  ,move_up_then_left
		  ,move_down_then_right
		  ,move_down_then_left
		  ]).


% ========================================
% Compound actions - double moves

%!	move_right_twice(+State,-New) is det.
%
%	Repeat a move_right/2 action twice.
%
move_right_twice(Ss,Gs):-
	move_right(Ss,Ss_2)
	,move_right(Ss_2,Gs).


%!	move_left_twice(+State,-New) is det.
%
%	Repeat a move_left/2 action twice.
%
move_left_twice(Ss,Gs):-
	move_left(Ss,Ss_2)
	,move_left(Ss_2,Gs).


%!	move_up_twice(+State,-New) is det.
%
%	Repeat a move_up/2 action twice.
%
move_up_twice(Ss,Gs):-
	move_up(Ss,Ss_2)
	,move_up(Ss_2,Gs).


%!	move_down_twice(+State,-New) is det.
%
%	Repeat a move_down/2 action twice.
%
move_down_twice(Ss,Gs):-
	move_down(Ss,Ss_2)
	,move_down(Ss_2,Gs).


% ========================================
% Compound actions - moves at an angle

%!	move_right_then_up(+State, -New) is det.
%
%	Combine	a move to the right with a move upwards.
%
move_right_then_up(Ss,Gs):-
	move_right(Ss,Ss_2)
	,move_up(Ss_2,Gs).


%!	move_right_then_down(+State,-New) is det.
%
%	Combine	a move to the right with a move downwards.
%
move_right_then_down(Ss,Gs):-
	move_right(Ss,Ss_2)
	,move_down(Ss_2,Gs).


%!	move_left_then_up(+State, -New) is det.
%
%	Combine	a move to the left with a move upwards.
%
move_left_then_up(Ss,Gs):-
	move_left(Ss,Ss_2)
	,move_up(Ss_2,Gs).


%!	move_left_then_down(+State,-New) is det.
%
%	Combine	a move to the left with a move downwards.
%
move_left_then_down(Ss,Gs):-
	move_left(Ss,Ss_2)
	,move_down(Ss_2,Gs).


%!	move_up_then_right(+State, -New) is det.
%
%	Combine	a move upwards with a move to the right.
%
move_up_then_right(Ss,Gs):-
	move_up(Ss,Ss_2)
	,move_right(Ss_2,Gs).


%!	move_up_then_left(+State, -New) is det.
%
%	Combine	a move to the left with a move upwards.
%
move_up_then_left(Ss,Gs):-
	move_up(Ss,Ss_2)
	,move_left(Ss_2,Gs).


%!	move_down_then_right(+State, -New) is det.
%
%	Combine	a move downwards with a move to the right.
%
move_down_then_right(Ss,Gs):-
	move_down(Ss,Ss_2)
	,move_right(Ss_2,Gs).


%!	move_down_then_left(+State, -New) is det.
%
%	Combine	a move to the left with a move downwards.
%
move_down_then_left(Ss,Gs):-
	move_down(Ss,Ss_2)
	,move_left(Ss_2,Gs).
