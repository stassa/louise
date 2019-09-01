:-module(robots, [background_knowledge/2
		 ,metarules/2
		 ,positive_example/2
		 ,negative_example/2
		 ,experiment/2
		 ,list_problem_world/1
		  %,stay/2
		 ,at_goal/1
		 ,move_right/2
		 ,move_left/2
		 ,move_up/2
		 ,move_down/2
		 ,move_right_twice/2
		 ,move_left_twice/2
		 ,move_up_twice/2
		 ,move_down_twice/2
		 ,move_right_then_up/2
		 ,move_right_then_down/2
		 ,move_left_then_up/2
		 ,move_left_then_down/2
		 ,move/5
		 ,within_limits/2
		 ]).

:-user:use_module(world).
:-user:use_module(render).


% ========================================
% Experiment logging
% ========================================


%!	close_log(+Alias) is det.
%
%	Close the log file with the given Alias if it exists.
%
close_log(A):-
	(   is_stream(A)
	->  close(robots)
	;   true
	) .


%!	debug_timestamp(-Timestamp) is det.
%
%	Helper predicate to generate a Timestamp for log files.
%
%	Note that this is different to logging_timestamp/1 in module
%	logging. The format of Timestamp in this predicate is suitable
%	for naming a file (i.e. no characters that give Windows the
%	fits) whereas the format in logging_timstamp/1 is appropriate
%	for printing in a log file.
%
debug_timestamp(A):-
	get_time(T)
	,stamp_date_time(T, DT, local)
	,format_time(atom(A), '%d_%m_%y_%H_%M_%S', DT).


% Comment out to disable logging to file.
%:-debug(robots).

% Uncomment to log debug outputs to a file. Note that debug messages to
% the console cease entirely and progress can't be tracked easily.
% Change mode from "write" to "append" to avoid clobbering earlier logs.
%
start_logging:-
	close_log(robots)
	,debug_timestamp(T)
	,atom_concat(robots_3,T,Bn)
	,atom_concat(Bn,'.log',Fn)
	,atom_concat('logs/robots/',Fn,P)
	% ,open('logs/robots_3.log',write,S,[alias(robots)])
	,open(P,write,S,[alias(robots)])
	,debug(robots>S).


%:-debug(robots>'logs/robot_exp_2.log').

% Uncomment to allow tracking progresss while logging to file.
%:-debug(progress).


% ========================================
% Experiment code
% ========================================

experiment(1,Rs):-
	experiment_world(World)
	,world_dimensions(W,H)
	,start_logging
	,list_problem(robots)
	,list_problem_world(robots)
	,nl(robots)
	,exp1(World,W,H,10,10,Rs)
	,debug(robots,'Results:',[])
	,debug(robots,'~w',[Rs])
	,close_log(robots).

%!	exp1(+World,+Width,+Height,+Steps,+Problems,-Results) is
%!	det.
%
%	Run an experiment counting successful learning attempts.
%
exp1(World,W,H,K,N,Rs):-
	background_knowledge(move/2,BK)
	,metarules(move/2,MS)
	,findall(Acc_
	       ,(between(0,K,K_)
		,debug(robots,'Step ~w of ~w',[K_,K])
		,Wk is W + K_
		,Hk is H + K_
		,findall(move(Ss,Gs)
			,problem(World,nondeterministic,Wk,Hk,Ss,Gs)
			,Ps)
		,length(Ps, Ps_N)
		,debug(robots,'World dimensions: ~w x ~w',[Wk,Hk])
		,debug(robots,'Got ~w problems', [Ps_N])
		,findall(Hs
			,(between(1,N,N_)
			 ,debug(robots
			       ,'Problem ~w of ~w in step ~w'
			       ,[N_,N,K_])
			 ,random_member(P,Ps)
			 ,flush_output
			 ,render_problem(P)
			 ,(   learn([P],[],BK,MS,Hs)
			     ,Hs \= []
			     ,Hs \= [P]
			  ->  length(Hs, H_N)
			     ,debug(robots,'Learned ~w clauses',[H_N])
			     ,debug_clauses(robots,Hs)
			     ,debug(robots,'',[])
			  ;   debug(robots,'Failed to learn a hypothesis.',[])
			     ,debug(robots,'',[])
			     ,fail
			 )
			 )
			,HS)
		,length(HS,C)
		,Acc is C / N
		,format(atom(Acc_),'~4f',Acc)
		)
	       ,Rs).


% ========================================
% Robot world setup
% ========================================

%!	experiment_world(?World) is semidet.
%
%	Name of the current experiment World.
%
%	Use to generate grids for positive examples.
%
experiment_world(empty_world).


%!	world_dimensions(?Width,?Height) is semidet.
%
%	Dimensions of a robot experiment world.
%
%	Used to generate examples for robot navigation plans.
%
world_dimensions(1,1).


%!	training_sample(?Sample) is semidet.
%
%	Size of training Sample for positive examples.
%
%	Used to select a subset of all grids generated in the current
%	experiment world.
%
%training_sample(0.000001).
training_sample(0.001).


%!	problems(+World,+Width,+Height,+Sample,-Problems) is det.
%
%	Collect a Sample of Problems from the given World.
%
%	Use this predicate to generate grid worlds and sample from them.
%
problems(World,W,H,S,Ps):-
	findall(move(Ss,Gs)
	       ,problem(World,nondeterministic,W,H,Ss,Gs)
	       ,Ps_)
	,p_list_samples(S,Ps_,Ps).


%!	list_problem_world(+Stream) is det.
%
%	List the parameters of problem world construction.
%
list_problem_world(St):-
	experiment_world(World)
	,world_dimensions(W,H)
	,training_sample(S)
	,format(St,'World: ~w~n', [World])
	,format(St,'Dimensions: ~w x ~w~n', [W,H])
	,format(St,'Training sample: ~w~n', [S])
	,problems(World,W,H,1.0,Ps)
	,length(Ps, N)
	,(   float(S)
	->   M is max(1, integer(S * N))
	 ;   integer(S)
	->   S_ is S/100
	    ,M is max(1, integer(S_ * N))
	 )
	,format(St,'Problems: ~D~n', [N])
	,format(St,'Sampled: ~D~n', [M]).



%!	list_problem(+Stream) is det.
%
%	List BK and metarules used in a robot experiment.
%
list_problem(St):-
	background_knowledge(move/2,BK)
	,metarules(move/2,MS)
	,format(St,'Background knowledge: ~w~n',[BK])
	,format(St,'Metarules: ~w~n',[MS]).


% ========================================
% MIL Problem
% ========================================

:-dynamic m/3
         ,m/2.

configuration:metarule(ground_identity,P,Q,X,Y):- m(P,X,Y), m(Q,X,Y).
configuration:metarule(ground_chain,P,Q,R,X,Y,Z):- m(P,X,Y), m(Q,X,Z), m(R,Z,Y).
configuration:metarule(postcon,P,Q,R,X,Y):- m(P,X,Y), m(Q,X,Y), m(R,Y).
configuration:metarule(unit_identity,P,Q):- m(P,X,_Y), m(Q,X).

background_knowledge(move/2, [%stay/2
			      at_goal/1
			     ,move_right/2
			     ,move_left/2
			     ,move_up/2
			     ,move_down/2
			     ,move_right_twice/2
			     ,move_left_twice/2
			     ,move_up_twice/2
			     ,move_down_twice/2
			     ,move_right_then_up/2
			     ,move_right_then_down/2
			     ,move_left_then_up/2
			     ,move_left_then_down/2
			     ,move/5
			     ,within_limits/2
			     ]).

%metarules(move/2,[identity,chain,precon,postcon]).
%metarules(move/2,[identity,chain,precon]).
%metarules(move/2,[ground_identity,ground_chain,ground_postcon]).
metarules(move/2,[unit_identity,chain,postcon,projection]).

positive_example(move/2,E):-
	experiment_world(World)
	,world_dimensions(W,H)
	,training_sample(S)
	,problems(World,W,H,S,Ps)
	,member(E, Ps).

negative_example(move/2,_):-
	fail.


% ========================================
% Background Knowledge
% ========================================


%!	at_goal(+State) is det.
%
%	True when the robot has reached its goal.
%
at_goal([G,G,W-H]):-
	ground([G,G,W-H]).


% ========================================
% Primitive actions


%!	stay(+State, -New) is det.
%
%	A move that doesn't change the robot's position.
%
stay(Ss,_Gs):-
	\+ ground(Ss)
	,!
	,fail.
stay(Ss,Ss).


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
