:-module(robots_gen, [background_knowledge/2
		     ,metarules/2
		     ,positive_example/2
		     ,negative_example/2
		     ,learning_rates/6
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

:-user:use_module('../world.pl').
:-user:use_module('../render.pl').
:-user:use_module(move_generator).
:- dataset_file_name(_,Fn)
  ,reexport(Fn).

:-dynamic m/3
         ,m/2.

%:-edit(move_generator).
%:-edit(generator_configuration).

:-debug(robots).

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
	,open(P,write,S,[alias(robots)])
	,debug(robots>S).

% Uncomment to allow tracking progresss while logging to file.
:-debug(progress).


% ========================================
% Experiment code
% ========================================


%!	learning_rates(+Target,+Metric,+Steps,+Samples,-Means,-SDs) is
%!	det.
%
%	Perform a learning rate experiment and collect Means and SDs.
%
%	Target is a predicate indicator, the symbol and arity of the
%	learning target, so move/2 for grid world navigation.
%
%	Metric is an atom, the metric to be measured: err, acc, fpr,
%	etc.
%
%	Steps is an integer, the number of steps the experiment will run
%	for.
%
%	Samples is a list of numbers, either integers or floats. In each
%	Step, a learning atempt is made for each number in Samples.
%
%	Means is a list of length equal to Samples where each element is
%	the mean value of the selected Metric for each Sample size at
%	the same position in Samples.
%
%	SDs is a list of length equal to Samples storing the standard
%	deviations of the reasults averaged in Means.
%
learning_rates(T,M,K,Ss,Ms,SDs):-
	configuration:learner(L)
	,start_logging
	,learning_rate(T,M,K,Ss,Rs)
	,Rs \= []
	,pairs_averages(Rs,Ms)
	,pairs_sd(Rs,Ms,SDs)
	% Print R vectors for plotting
	,Ms_v =.. [c|Ms]
	,SDs_v =.. [c|SDs]
	,debug(robots,'~w.acc.mean <- ~w',[L,Ms_v])
	,debug(robots,'~w.acc.sd <- ~w',[L,SDs_v])
	,close_log(robots).

%!	learning_rate(+Target,+Metric,+Steps,+Samples,-Results) is det.
%
%	Business end of learning_rates/6.
%
%	Results is a list of lists of length equal to Samples, where
%	each sub-list is the lits of values of the chosen Metric for
%	the corresponding Sample size.
%
learning_rate(T,M,K,Ss,Rs):-
	findall(Vs
	       ,(between(1,K,J)
		,debug(progress,'Step ~w of ~w',[J,K])
		,findall(S-V
			,(member(S,Ss)
			 ,debug(progress,'Sampling size: ~w',[S])
			 % Soft cut to stop Thelma backtracking
			 % into multiple learning steps.
			 ,once(train_and_test(T,S,_Ps,M,V))
			 )
			,Vs)
		)
	       ,Rs).


% ========================================
% MIL problem
% ========================================

configuration:metarule(unit_identity,P,Q):- m(P,X,_Y), m(Q,X).
configuration:metarule(unit_identity, [P,Q], [X,Y], mec(P,X,Y) :- mec(Q,X)).

background_knowledge(move/2, [% Move primitives
			      move_right/2
			     ,move_left/2
			     ,move_up/2
			     ,move_down/2
			      % Location primitives
			     ,start/1
			     ,end/1
			      % Compound actions - double moves
			     ,move_right_twice/2
			     ,move_left_twice/2
			     ,move_up_twice/2
			     ,move_down_twice/2
			      % Compound actions - angled moves
			     %,move_right_then_up/2
			     %,move_right_then_down/2
			     %,move_left_then_up/2
			     %,move_left_then_down/2
			     %,move_up_then_right/2
			     %,move_up_then_left/2
			     %,move_down_then_right/2
			     %,move_down_then_left/2
			  ]).


metarules(move/2,[unit_identity,chain,precon,postcon,projection]).

positive_example(move/2,move(Ss,Gs)):-
	dataset_file_name(Bn,_)
	,Bn:move(Ss,Gs).

negative_example(move/2,_):-
	fail.


% ========================================
% Background knowledge definitions (local)
% ========================================


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

