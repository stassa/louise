:-module(kinship_accuracy, [background_knowledge/2
			   ,metarules/2
			   ,positive_example/2
			   ,negative_example/2
			   ,grandparent/2
			   ,grandfather/2
			   ,grandmother/2
			   ,parent/2
			   ,aunt/2
			   ,uncle/2
			   ,cousin/2
			   ,nephew/2
			   ,niece/2
			   ,child/2
			   ,son/2
			   ,daughter/2
			   ,brother/2
			   ,sister/2
			   ,father/2
			   ,mother/2
			   ,male/1
			   ,female/1
			   ,married/2
			   ,learning_rates/3
			   ]).


/* For experiments with Louise, when learning blood_relative/2, set:
   derivation_depth(9).
   resolutions(250_000).

Derivation depth is a bit more forgiving, but a high number of
resolutions is needed to get the best reduction results in the minimum
amount of time. Actually, 250,000 are not sufficient to fully reduce the
learned hypothesis- there is still some redundancy left. To be honest, I
don't know how high resolutions/1 should go to fully reduce the theory.
Bit of a weakness of the procedure there :/

*/

%==============================================================================
%   EXPERIMENT SETTINGS
%==============================================================================

%!	sample_size(?Size) is semidet.
%
%	Sampling size for learning rate experiment.
%
sample_size(0.5).


%!	time_limit(?Seconds) is semidet.
%
%	Time limit in Seconds for learning rate experiment.
%
time_limit(5).


%!	logs_path(?Path) is semidet.
%
%	Path to the logs/ subdirectory for experiments in this file.
%
logs_path('logs/kinship/').


%!	debug_stream_alias(?Alias) is semidet.
%
%	Alias of the log stream for experiments in this file.
%
debug_stream_alias(learning_rate).


%==============================================================================
% EXPERIMENT LOGGING
%==============================================================================

% Uncomment to allow tracking progresss while logging to file.
:-debug(progress).

%!	start_logging(+Cycles,+Metric) is det.
%
%	Start logging to the current log file.
%
start_logging(K,M):-
	configuration:learner(L)
	,debug_stream_alias(A)
	,logs_path(LP)
	,close_log(learning_rate)
	,debug_timestamp(T)
	,atomic_list_concat([L,learning_rate,M,K,T],'_',Bn)
	,atom_concat(Bn,'.log',Fn)
	,atom_concat(LP,Fn,P)
	,open(P,write,S,[alias(A)])
	,debug(A>S).


%!	close_log(+Alias) is det.
%
%	Close the log file with the given Alias if it exists.
%
close_log(A):-
	(   is_stream(A)
	->  close(A)
	;   true
	) .


%!	debug_timestamp(-Timestamp) is det.
%
%	Helper predicate to generate a Timestamp for log files.
%
debug_timestamp(A):-
	get_time(T)
	,stamp_date_time(T, DT, local)
	,format_time(atom(A), '%d-%m-%y_%H-%M-%S', DT).


%!	log_options(+Cycles,+Target,+Metric) is det.
%
%	Log experiment settings.
%
log_options(K,T,M):-
	configuration:learner(Lrn)
	,debug_stream_alias(A)
	,sample_size(S)
	,time_limit(L)
	,debug(A,'Experiment settings',[])
	,debug(A,'Learner: ~w',[Lrn])
	,debug(A,'Learning target: ~w',[T])
	,debug(A,'Evaluation metric: ~w',[M])
	,debug(A,'Experiment cycles: ~w',[K])
	,debug(A,'Sample size: ~w',[S])
	,debug(A,'Time limit: ~w~n~n',[L]).


%!	log_r_vectors(+Metric,+Means,+SDs) is det.
%
%	Add R vectors for plotting to experiment log.
%
log_r_vectors(M,Ms,SDs):-
	configuration:learner(L)
	,Ms_v =.. [c|Ms]
	,SDs_v =.. [c|SDs]
	,debug(learning_rate, 'metric <- \'~w\'', [M])
	,debug(learning_rate,'~w.~w.mean <- ~w',[L,M,Ms_v])
	,debug(learning_rate,'~w.~w.sd <- ~w',[L,M,SDs_v]).


%==============================================================================
% EXPERIMENT CODE
%==============================================================================

%!	learning_rates(+K,+Target,+Metric) is det.
%
%	Debug results of K learning rate experiment cycles.
%
%	Prints results of the experiment to the current log file.
%
%	Metric is the metric to use for performance evaluation.
%
learning_rates(K,T,M):-
	debug_stream_alias(A)
	,start_logging(K,M)
	,log_options(K,T,M)
	,findall(Rs_
		,(between(1,K,I)
		 ,debug(progress,'Cycle ~w of ~w',[I,K])
		 ,debug(learning_rate,'Cycle ~w of ~w',[I,K])
		 ,learning_rate(T,M,Rs_i)
		 ,findall(J-V
			 ,nth1(J,Rs_i,r(_Ps,_N,V))
			 ,Rs_)
		 )
		,Rs)
	,Rs \= []
	,pairs_averages(Rs,As)
	,pairs_sd(Rs,As,SDs)
	,debug(learning_rate
	      ,'Theory size and training times (sec.) for ~w cycles.',[K])
	,debug(learning_rate,'|Th| Training time (mean) Training Time (SD)',[])
	,forall(nth1(I,As,A_i)
	       ,(nth1(I,SDs,Sd_i)
		,debug(learning_rate
		      ,'~2|~`0t~d~2+ ~3+~4f~20+ ~2t~4f~20t',[I,A_i,Sd_i])
		)
	       )
	,debug(learning_rate,'~n',[])
	,log_r_vectors(M,As,SDs)
	,close_log(A).


%!	learning_rate(+Target,+Metric,-Results) is det.
%
%	Perform	a learning rate experiment.
%
%	Target is the predicate indicator of the learning target. Result
%	is a list of key-value pairs, N-T, where each key, N, is the
%	size of a hypothesis of Target (in clauses) and each value, T,
%	is the time it took to learn that hypothesis, measured in
%	seconds.
%
%	Metric is the atomic name of one of the evaluation metrics
%	defined in module evaluation: acc, err, fpr, fnr, ertc.
%
%	The learning rate experiment performed by this predicate
%	incrementally selects clauses of the target theory for Target,
%	at random and without replacement, adds them to an initially
%	empty sub-theory and uses this sub-theory to generate positive
%	examples and find background knowledge for a training atempt.
%	Background knowledge is found by taking the symbols and arities
%	of body literals in all clauses of Target in the Target's
%	definition in the current step.
%
learning_rate(T,M,Rs):-
	sample_size(S)
	,time_limit(L)
	,program(T,kinship_accuracy,Ts)
	,experiment_data(T,_,Neg,_BK,MS)
	,learning_rate(T,S,L,M,[],Ts,Neg,MS,[],Rs_)
	,findall(r(Ps,N,V)
		,(member(V-Ps,Rs_)
		 ,length(Ps,N)
		 )
		,Rs).

%!	learning_rate(+Trgt,+Samp,+Lim,+Current,+Th,+Neg,+Meta,+Acc,-Results)
%	is det.
%
%	Business end of learning_rate/2.
%
learning_rate(_T,_S,_L,_M,Ts_i,[],_Neg,_MS,Acc,Rs):-
	Ts_i \= []
	,reverse(Acc,Rs)
	,!.
learning_rate(T,S,L,M,Ts_i,Ts,Neg,MS,Acc,Bind):-
	random_select(C,Ts,Ts_)
	,theory_examples_bk([C|Ts_i],Pos,BK)
	,length([C|Ts_i],Ts_n)
	,debug(progress,'Processing step ~w',[Ts_n])
	,debug(learning_rate,'Target theory:', [])
	,debug_clauses(learning_rate,[C|Ts_i])
	,debug(learning_rate,'BK:~w', [BK])
	,length(Pos, Pos_n)
	,debug(learning_rate,'Positive examples:~w', [Pos_n])
	,timed_train_and_test(T,S,L,[Pos,Neg,BK,MS],Ps,M,V)
	,!
	,length(Ps,Ps_n)
	,debug(learning_rate,'Learned a ~w-clause hypothesis.',[Ps_n])
	,debug_clauses(learning_rate,Ps)
	,debug(learning_rate,'',[])
	,learning_rate(T,S,L,M,[C|Ts_i],Ts_,Neg,MS,[V-Ps|Acc],Bind).
learning_rate(T,S,L,M,Ts_i,_Ts,Neg,MS,Acc,Bind):-
	length(Ts_i,N)
	,debug(learning_rate,'Learning failed after ~w clauses.',[N])
	,learning_rate(T,S,L,M,Ts_i,[],Neg,MS,Acc,Bind).


%!	theory_examples_bk(+Target,-Positive,-BK) is det.
%
%	Generate Positive examples and BK for Target.
%
theory_examples_bk(Ts,Pos_,BK):-
	theory_symbols(Ts,BK)
	,program(BK,kinship_accuracy,BK_Ps)
	,lfp(BK_Ps,BK_As)
	,lfp_query(Ts,BK_As,_Is,Pos)
	,sort(Pos, Pos_).


%!	theory_symbols(+Theory,-Symbols) is det.
%
%	Collect the predicate Symbols of body literals in Theory.
%
theory_symbols(Ts,Ss):-
	theory_symbols(Ts,[],Ss).

%!	theory_symbols(+Theory,+Acc,-Symbols) is det.
%
%	Business end of theory_symbols/2
%
theory_symbols(Ts,Acc,Bind):-
	findall(F/A
	       ,(member(_H:-B,Ts)
		,once(list_tree(Bs,B))
		,member(P,Bs)
		,functor(P,F,A)
		)
	       ,Ss)
	% No symbols if Ts is a list of atoms only.
	,Ss \= []
	,!
	,program(Ss,kinship_accuracy,Ts_)
	,theory_symbols(Ts_,[Ss|Acc],Bind).
theory_symbols(_Ts,Acc,Ss):-
	flatten(Acc,Ss_)
	,sort(Ss_,Ss).


%==============================================================================
% MIL PROBLEM
%==============================================================================

background_knowledge(blood_relative/2,[grandparent/2
				      ,grandfather/2
				      ,grandmother/2
				      ,parent/2
				      ,aunt/2
				      ,uncle/2
				      ,cousin/2
				      ,nephew/2
				      ,niece/2
				      ,child/2
				      ,son/2
				      ,daughter/2
				      ,brother/2
				      ,sister/2
				      ,father/2
				      ,mother/2
				      ,male/1
				      ,female/1
				      ]).

background_knowledge(unrelated/2,[grandparent/2
				 ,grandfather/2
				 ,grandmother/2
				 ,parent/2
				 ,aunt/2
				 ,uncle/2
				 ,cousin/2
				 ,nephew/2
				 ,niece/2
				 ,child/2
				 ,son/2
				 ,daughter/2
				 ,brother/2
				 ,sister/2
				 ,father/2
				 ,mother/2
				 ,male/1
				 ,female/1
				 ,married/2
				 ]).


metarules(blood_relative/2,[identity,inverse]).
metarules(unrelated/2,[identity,inverse,chain]).

positive_example(blood_relative/2,blood_relative(X,Y)):-
	blood_relative(X,Y).
positive_example(unrelated/2,unrelated(X,Y)):-
	unrelated(X,Y).

negative_example(blood_relative/2,blood_relative(X,Y)):-
	unrelated(X,Y).
negative_example(unrelated/2,unrelated(X,Y)):-
	blood_relative(X,Y).


% Target theory for unrelated/2. Also used to generate negative examples
% for blood_relative/2.
unrelated(X,Y):-
	individual(X)
	,individual(Y)
	,\+ blood_relative(X,Y)
	,\+ blood_relative(Y,X).

individual(X):-
	male(X).
individual(X):-
	female(X).


% Target theory for blood relative/2. Also used to generate negative
% examples for unrelated/2.
blood_relative(X,Y):- grandparent(X,Y).
blood_relative(X,Y):- grandfather(X,Y).
blood_relative(X,Y):- grandmother(X,Y).
blood_relative(X,Y):- parent(X,Y).
blood_relative(X,Y):- aunt(X,Y).
blood_relative(X,Y):- uncle(X,Y).
blood_relative(X,Y):- cousin(X,Y).
blood_relative(X,Y):- nephew(X,Y).
blood_relative(X,Y):- niece(X,Y).
blood_relative(X,Y):- child(X,Y).
blood_relative(X,Y):- son(X,Y).
blood_relative(X,Y):- daughter(X,Y).
blood_relative(X,Y):- brother(X,Y).
blood_relative(X,Y):- sister(X,Y).
blood_relative(X,Y):- father(X,Y).
blood_relative(X,Y):- mother(X,Y).
% blood_relative/2 is reflexive.
blood_relative(X,Y):- grandparent(Y,X).
blood_relative(X,Y):- grandfather(Y,X).
blood_relative(X,Y):- grandmother(Y,X).
blood_relative(X,Y):- parent(Y,X).
blood_relative(X,Y):- aunt(Y,X).
blood_relative(X,Y):- uncle(Y,X).
blood_relative(X,Y):- cousin(Y,X).
blood_relative(X,Y):- nephew(Y,X).
blood_relative(X,Y):- niece(Y,X).
blood_relative(X,Y):- child(Y,X).
blood_relative(X,Y):- son(Y,X).
blood_relative(X,Y):- daughter(Y,X).
blood_relative(X,Y):- brother(Y,X).
blood_relative(X,Y):- sister(Y,X).
blood_relative(X,Y):- father(Y,X).
blood_relative(X,Y):- mother(Y,X).


% BK definitions - blood relations
grandparent(X,Y):- grandfather(X,Y).
grandparent(X,Y):- grandmother(X,Y).
grandfather(X,Y):- father(X,Z), parent(Z,Y).
grandmother(X,Y):- mother(X,Z), parent(Z,Y).
parent(X,Y):- father(X,Y).
parent(X,Y):- mother(X,Y).
aunt(X,Y):- sister(X,Z),parent(Z,Y).
uncle(X,Y):- brother(X,Z),parent(Z,Y).
cousin(X,Y):-child(X,Z),aunt(Z,Y).
cousin(X,Y):-child(X,Z),uncle(Z,Y).
nephew(X,Y):- male(X),aunt(Y,X).
nephew(X,Y):- male(X),uncle(Y,X).
niece(X,Y):- female(X),aunt(Y,X).
niece(X,Y):- female(X),uncle(Y,X).
child(X,Y):- son(X,Y).
child(X,Y):- daughter(X,Y).
son(X,Y):- male(X),parent(Y,X).
daughter(X,Y):- female(X),parent(Y,X).
%brother(X,Y):- son(X,Z), parent(Z,Y), X \= Y.
%sister(X,Y):- daughter(X,Z), parent(Z,Y), X \= Y.
brother(X,Y):- son(X,Z), parent(Z,Y).
sister(X,Y):- daughter(X,Z), parent(Z,Y).

father(stathis, kostas).
father(stathis, gioula).
father(stefanos, miltos).
father(stefanos, akis).
father(stefanos, theodora).
father(kostas, stassa).
father(miltos, stefanakis).
father(akis, kostis).
father(vassilis, nikolas).
father(vassilis, alexandros).

mother(alexandra, kostas).
mother(alexandra, gioula).
mother(voula, miltos).
mother(voula, akis).
mother(voula, theodora).
mother(ada, stefanakis).
mother(theodora, stassa).
mother(efi, kostis).
mother(gioula, nikolas).
mother(gioula, alexandros).

male(stathis).
male(stefanos).
male(kostas).
male(vassilis).
male(akis).
male(miltos).
male(stefanakis).
male(nikolas).
male(kostis).
male(alexandros).

female(alexandra).
female(voula).
female(theodora).
female(gioula).
female(ada).
female(efi).
female(stassa).

% BK definitions - not blood relations.
married(akis,efi).
married(kostas,theodora).
married(miltos,ada).
married(stathis,alexandra).
married(stefanos,voula).
married(vassilis,gioula).
% Extensional reflexion of the "married" relation
married(ada,miltos).
married(alexandra,stathis).
married(efi,akis).
married(gioula,vassilis).
married(theodora,kostas).
married(voula,stefanos).
