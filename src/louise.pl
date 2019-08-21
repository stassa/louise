:-module(louise, [learn_episodic/1
		 ,learn_episodic/2
		 ,learn_episodic/5
		 ,learn/1
		 ,learn/2
		 ,learn/5
		 ,top_program/6
		 ,reduced_top_program/6
		 ,lfp_query/4
		 ,lfp_query/3
		 ,lfp/2
		 ]).

:-use_module(configuration).
:-use_module(mil_problem).


%!	learn_episodic(+Target) is det.
%
%	Learn a definition of a Target in successive episodes.
%
learn_episodic(T):-
	learn_episodic(T,Ps)
	,print_clauses(Ps).



%!	learn_episodic(+Target,-Definition) is det.
%
%	Learn a Definition of a Target in successive episodes.
%
learn_episodic(T,Ps):-
	tp_safe_experiment_data(T,Pos,Neg,BK,MS)
	,learn_episodic(Pos,Neg,BK,MS,Ps).


%!	tp_safe_experiment_data(+Target,-Pos,-Neg,-BK,-MS) is det.
%
%	Ensure experiment data is safe for TP operator predicates.
%
%	Basically just removes the ":-" in front of the negative
%	examples, since they are not recognised by the Top program
%	construction predicates that use a TP operator.
%
tp_safe_experiment_data(T,Pos,Neg_,BK,MS):-
	configuration:theorem_prover(TP)
	,experiment_data(T,Pos,Neg,BK,MS)
	,(   TP == tp
	 ->  setof(E
		  ,Neg^member((:-E),Neg)
		  ,Neg_)
	 ;   Neg_ = Neg
	 )
	,!.
tp_safe_experiment_data(T,Pos,[],BK,MS):-
% If there are no negative examples there's nothing to sanitise.
	experiment_data(T,Pos,[],BK,MS).



%!	learn_episodic(+Pos,+Neg,+BK,+Metarules,-Program) is det.
%
%	Learn a Program over successive episodes.
%
%	Base predicate for episodic learning. Program is learned in
%	successive episodes where the learned hypothesis is added to the
%	BK and learning begins all over again.
%
learn_episodic(Pos,Neg,BK,MS,Ps):-
	debug(episodic,'Encapsulating problem',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_,Ss])
	,debug(episodic,'Learning first episode',[])
	,learning_episode(Pos_,Neg_,BK_,MS_,Ss,Ps_1)
	,examples_target(Pos,T)
	,learned_hypothesis(T,Ps_1,Es_1)
	,length(Es_1,N)
	,learn_episodic(T,N,Pos_,Neg_,BK_,MS_,Ss,Es_1,Ps_k)
	,excapsulated_clauses(T,Ps_k,Ps).


%!	learned_hypothesis(+Target,+Program,-Hypothesis) is det.
%
%	Collect the clauses of a learned Hypothesis.
%
%	Helper to separate a learned hypothesis from the rest of a
%	reduced program, so that it can be added to the background
%	knowledge for a subsequent learning episode.
%
learned_hypothesis(T/A,Ps,Hs):-
	findall(H:-B
	       ,(member(H:-B,Ps)
		,H =.. [m,T|As]
		,length(As,A)
		)
	       ,Hs).


%!	learn_episodic(+Target,+N,+Pos,+Neg,+BK,+Meta,+Sig,+Acc,-Bind)
%!	is det.
%
%	Business end of learn_episodic/5.
%
%	Recursively learns a hypothesis with background knowledge
%	including the hypothesis learned in the previous recursion step.
%
%	Recursion stops when the length of the learned hypothesis does
%	not change from one recursion step to the next.
%
learn_episodic(T/A,N,Pos,Neg,BK,MS,Ss,Es_i,Bind):-
	append(BK,Es_i,BK_)
	,debug(episodic,'Learning new episode',[])
	,learning_episode(Pos,Neg,BK_,MS,Ss,Ps)
	,learned_hypothesis(T/A,Ps,Es_j)
	,length(Es_j,M)
	,M > N
	,!
	,learn_episodic(T/A,M,Pos,Neg,BK_,MS,Ss,Es_j,Bind).
learn_episodic(_T,_M,_Pos,_Neg,_BK,_MS,_Ss,Ps,Ps).


%!	learning_episode(+Pos,+Neg,+BK,+Ms,+Sig,-Episode) is det.
%
%	Process one learning episode.
%
%	One learning episode consists of constructing the Top program
%	and then reducing it.
%
%	@tbd This could replace the two calls to top_program/6 and
%	reduced_top_program/6 in learn/5, so as to add the recursion
%	depth limit test in here. Just in case.
%
learning_episode(Pos,Neg,BK,MS,Ss,Es):-
	configuration:theorem_prover(TP)
	,configuration:recursion_depth_limit(episodic_learning,L)
	,debug(episodic,'Constructing Top program',[])
	,G = top_program(Pos,Neg,BK,MS,Ss,Ms)
	,recursion_guard(G,L,TP)
	,debug(episodic,'Reducing Top program',[])
	,reduced_top_program(Pos,BK,MS,Ss,Ms,Es).


%!	recursion_guard(+Goal,+Time_Limit,+Theorem_Prover) is det.
%
%	Call Goal guarding for infinite recursion.
%
%	Time_Limit is passed to call_with_depth_limit/3 if necessary.
%
%	Theorem_Prover is the current value of the configuration option
%	theorem_prover/1. If this is "resolution" then Depth_Limit is
%	used with call_with_depth_limit/3. If theorem_prover/1 is set to
%	"tp" there is no reason to guard against recursion her: the TP
%	operator is guaranteed to terminate. At least it is, given a
%	definite datalog program.
%
recursion_guard(G,L,resolution):-
	!
	,call_with_depth_limit(G,L,Rs)
	,Rs \= depth_limit_exceeded.
recursion_guard(G,_L,tp):-
% TP operator is already recursion-safe.
	call(G).



%!	learn(+Target) is det.
%
%	Learn a deafinition of a Target predicate.
%
learn(T):-
	learn(T,Ps)
	,print_clauses(Ps).



%!	learn(+Target,-Definition) is det.
%
%	Learn a definition of a Target predicate.
%
learn(T,Ps):-
	tp_safe_experiment_data(T,Pos,Neg,BK,MS)
	,learn(Pos,Neg,BK,MS,Ps).



%!	learn(+Pos,+Neg,+BK,+Metarules,-Progam) is det.
%
%	Learn a Progam from a MIL problem.
%
learn(Pos,Neg,BK,MS,Ps):-
	debug(learn,'Encapsulating problem',[])
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_,Ss])
	,debug(learn,'Constructing Top program',[])
	,top_program(Pos_,Neg_,BK_,MS_,Ss,Ms)
	,debug(learn,'Reducing Top program',[])
	,reduced_top_program(Pos_,BK_,MS_,Ss,Ms,Rs)
	,examples_target(Pos,T)
	,debug(learn,'Excapsulating problem',[])
	,excapsulated_clauses(T,Rs,Ps).



%!	top_program(+Pos,+Neg,+BK,+Metarules,+Signature,-Top) is det.
%
%	Construct the Top program for a MIL problem.
%
%	Clauses are selected according to the configuration setting
%	theorem_prover/1.
%
%	If theorem_prover/1 is set to "resolution", Top program
%	construction is performed in a top-down manner using SLD
%	resolution to decide entailment, which is faster (because it
%	hands off to the Prolog interpreter), but not guaranteed to
%	terminate (for example, it may go infinite given the
%	left-recursive nature of encapsulated metarules).
%
%	If the value of theorem_prover/1 is "tp", Top program
%	construction is performed in a bottom-up manner, using a TP
%	operator. This is slower (because it's implemented in Prolog)
%	but it's guaranteed to terminate. Note also that the TP operator
%	only works for datalog definite programs.
%
%	@bug Top program specialisation using the TP operator is still a
%	work in progress and may not fully eliminate too-general
%	metasubstitutions.
%
top_program(Pos,Neg,BK,MS,Ss,Ts):-
	configuration:theorem_prover(resolution)
	,!
	,write_program(Pos,BK,MS,Ss,Refs)
	,top_program(Pos,Neg,BK,MS,Ms)
	,unfolded_metasubs(Ms,Ts)
	,erase_program_clauses(Refs).
top_program(Pos,Neg,BK,MS,Ss,Ts):-
	configuration:theorem_prover(tp)
	,examples_target(Pos,T)
	,bind_target(MS,T,MS_)
	,flatten([Ss,Pos,BK],Ps)
	,generalise(MS_,Ps,Is,Ts_Pos)
	,unfolded_metasubs(Ts_Pos,Ts_Pos_)
	,specialise(Ts_Pos_,Is,Neg,Ts).


%!	write_program(+Pos,+BK,+MS,+PS,-Refs) is det.
%
%	Write an encapsulated program to the dynamic database.
%
%	@tbd The negative examples don't need to be written to the
%	dynamic database.
%
write_program(Pos,BK,MS,Ss,Rs):-
	findall(Rs_i
		,(member(P, [Pos,BK,MS,Ss])
		 ,assert_program(user,P,Rs_i)
		 )
		,Rs_)
	,flatten(Rs_,Rs).


%!	top_program(+Positive,+Negative,+BK,+Metarules,-Metasubstitutions)
%!	is det.
%
%	Collect all correct Metasubstitutions in a MIL problem.
%
top_program(Pos,Neg,_BK,MS,Ss):-
	generalise(Pos,MS,Ss_Pos)
	,specialise(Ss_Pos,Neg,Ss).


%!	generalise(+Positive,+Metarules,-Generalised) is det.
%
%	Generalisation step of Top program construction.
%
%	Generalises a set of Positive examples by finding each
%	metasubstitution of a metarule that entails a positive example.
%
generalise(Pos,MS,Ss_Pos):-
	setof(H
	     ,M^MS^Ep^Pos^(member(M,MS)
			  ,member(Ep,Pos)
			  ,metasubstitution(Ep,M,H)
			  )
	     ,Ss_Pos).

/* Alternative version- only resolves metarules, without taking into
%  account the examples except to bind the symbol of the target predicate.
%  This one is a tiny bit faster but the one above is currently the one
%  in the technical report on Louise.

generalise(Pos,MS,Ss_Pos):-
	Pos = [E|_Es]
	,E =.. [m,T|_As]
	,setof(M
	     ,M^B^MS^N^T^Ps^
	       (member(M:-B,MS)
		     ,M =.. [m,N,T|Ps]
		     ,call(M)
		     )
	     ,Ss_Pos).
*/


%!	specialise(+Generalised,+Negatives,-Specialised) is det.
%
%	Specialisation step of Top program construction.
%
%	Specialises a set of metasubstitutions generalising the positive
%	examples against the Negative examples by discarding each
%	metasubstitution that entails a negative example.
%
specialise(Ss_Pos,Neg,Ss_Neg):-
	setof(H
	     ,Ss_Pos^En^Neg^M^
	      (member(H,Ss_Pos)
	      ,\+((member(En,Neg)
		  ,metasubstitution(En,M,H)
		  )
		 )
	      )
	     ,Ss_Neg).


%!	metasubstitution(+Example,+Metarule,-Metasubstitution) is
%!	nondet.
%
%	Perform one Metasubstutition of Metarule initialised to Example.
%
%	Example is either a positive example or a negative example. A
%	positive example is a ground definite unit clause, while a
%	negative example is a ground definite goal (i.e. a clause of the
%	form :-Example).
%
metasubstitution(:-E,M,H):-
	!
	,bind_head_literal(E,M,(H:-(Ps,(E,Ls))))
	,metarule_expansion(_Id,(H:-(Ps,(E,Ls))))
	,user:call(Ps)
	,user:call(Ls).
metasubstitution(E,M,H):-
	bind_head_literal(E,M,(H:-(Ps,(E,Ls))))
	,user:call(Ps)
	,user:call(Ls).


%!	bind_head_literal(+Example,+Metarule,-Head) is det.
%
%	Bind an Example to the encapsulated Head literal of a Metarule.
%
%	Abstracts the complex patterns of binding examples to the heads
%	of metarules with and without body literals.
%
bind_head_literal(E,M,(H:-(Ps,(E,Ls)))):-
	M = (H:-(Ps,(E,Ls)))
	,!.
bind_head_literal(E,M,(H:-(Ps,(E,true)))):-
	M = (H:-(Ps,E)).


%!	bind_target(+Metarules,+Target,-Bound) is det.
%
%	Bind the Target's symbol to the heads of Metarules.
%
%	Small optimisation to ensure that lefp/2 only considers
%	metasubstitutions where the target predicate is the first
%	predicate symbol, rather than all possible metasubstitutions.
%
bind_target(MS,T/_A,MS_):-
	findall(H:-B
	       ,(member(H:-B, MS)
		,H =.. [m,_Id,T|_Ps]
		)
	       ,MS_).


%!	generalise(+Metarules,+Program,-Model,-Generalised) is
%!	det.
%
%	Top program generalisation step with TP operator.
%
%	Metarules is the set of metarules in the problem, expanded and
%	with the first predicate symbol in their head bound to the
%	symbol of the target predicate, as returned by bind_target/3.
%
%	Program is a flat list of the predicate signature and the
%	encapsulation of the positive examples and background knowledge.
%
%	Model is the least Herbrand model of Program. This is passed to
%	specialise/4 to avoid duplicating work (specifically, the work
%	of building this one up again).
%
%	@tbd This is just a thin wrapper around lfp_query/4 now. Still,
%	keep it around - makes it clear this is a distinct step in the
%	construction of the Top program.
%
generalise(MS,Ps,Is,Ts_Pos):-
	lfp_query(MS,Ps,Is,Ts_Pos)
	%,writeln('Top program - generalise:')
	%,print_clauses(Ts_Pos)
	%,nl
	%,writeln('Top program - Interpretation')
	%,print_clauses(Is)
	%,nl
	.


%!	specialise(+Generalised,+Model,+Negatives,-Specialised) is
%!	det.
%
%	Top program specialisation step with TP operator.
%
%	Generalised is the result of generalise/4. Model is the least
%	Herbrand model of the predicate signature, positive examples and
%	background knowledge, calculated during execution
%	of generalise/4.
%
specialise(Ts_Pos,Ps,Neg,Ts_Neg):-
	findall(H:-B
		,(member(H:-B,Ts_Pos)
		 ,lfp_query([H:-B],Ps,As)
		 ,ord_intersection(As, Neg, [])
		 )
		,Ts_Neg)
	%,writeln('Top program - specialise:')
	%,print_clauses(Ts_Neg)
	%,nl
	.



%!	reduced_top_program(+Pos,+BK,+Metarules,+Sig,+Program,-Reduced)
%!	is det.
%
%	Reduce the Top Program.
%
%	Clauses are selected according to the value of the configuration
%	option recursive_reduction/1. If this is set to true, the Top
%	program is reduced recursively, by passing the output of each
%	reduction step to the next, as input. If recursive_reduction/1
%	is set to false a single reduction step is performed.
%
%	Recursive reduction is useful when the Top program is large, or
%	recursive, and a large number of resolution steps are required
%	to reduce it effectively. In such cases, recursive reduction can
%	result in a stronger reduction of the Top program (i.e. result
%	in fewer redundant clauses in the learned hypothesis) in a
%	shorter amount of time, without increasing the number of
%	resolution steps in the program reduction meta-interpreter.
%
reduced_top_program(Pos,BK,MS,Ss,Ps,Rs):-
	configuration:recursive_reduction(true)
	,!
	,flatten([Ss,Pos,BK,Ps,MS],Fs_)
	,program_reduction(Fs_,Rs_,_)
	,length(Fs_,M)
	,length(Rs_,N)
	,debug(reduction,'Initial reduction: ~w to ~w',[M,N])
	,reduced_top_program_(N,Rs_,BK,MS,Ss,Rs)
	% program_reduction module leaves behind garbage
	% in program module. Why?
	,cleanup_experiment.
reduced_top_program(Pos,BK,MS,Ss,Ps,Rs):-
	configuration:recursive_reduction(false)
	,flatten([Ss,Pos,BK,Ps,MS],Fs_)
	,program_reduction(Fs_,Rs,_)
	,cleanup_experiment.


%!	reduced_top_program_(+N,+Prog,+BK,+Metarules,+Sig,-Reduced) is
%!	det.
%
%	Business end of reduced_top_program/6
%
%	Recursively reduces the Top Program, by feeding back the result
%	of each call to program_reduction/2 to itself, a process known
%	as "doing feedbacksies".
%
reduced_top_program_(N,Ps,BK,MS,Ss,Bind):-
	program_reduction(Ps,Rs,_)
	,length(Rs, M)
	,debug(reduction,'New reduction: ~w to ~w',[N,M])
	,M < N
	,!
	,reduced_top_program_(M,Rs,BK,MS,Ss,Bind).
reduced_top_program_(_,Rs,_BK,_MS,_Ss,Rs).




%!	lfp_query(+Query,+Program,-Interpretation,-Result) is det.
%
%	Answer a Query in the context of a Program.
%
%	First the least Herbrand Model of Program is calculated (by a
%	call to lfp/2), then the least Herbrand Model of Query is
%	calculated in the context of the resulting Interpretation,
%	yielding Result.
%
%	Result is constructed so as to exclude all the
%	atoms already in the Interpretation (even though strictly
%	speaking, the atoms in the Interpretation itself are in the
%	least Herbrand model of the Query given the Interpretation).
%
%	Use this predicate to separate the consequences of one program,
%	the Query, from another, the intended Interpretation.
%
lfp_query(Qs,Ps,Is,Rs):-
	lfp(Ps,Is)
	,lfp_query(Qs,Is,Rs).



%!	lfp_query(+Query,+Interpretation,-Result) is det.
%
%	Answer a Query in the context of an Interpreation.
%
%	As lfp_query/3 but does not calculate an Interpretation from an
%	input program, instead taking an already caculated
%	Interpretation as input.
%
lfp_query(Qs,Is,Rs):-
	lfp_query_(Qs,Is,[],Rs).


%!	lfp_query(+Query,+Int_Acc,+Res_Acc,-Result) is det.
%
%	Business end of lfp_query/4.
%
lfp_query_(Ps,Is_i,Rs_i,Bind_Rs):-
	tp_query(Ps,Is_i,Is_k,Rs_i,Rs_k)
	,Rs_i \= Rs_k
	,!
	,lfp_query_(Ps,Is_k,Rs_k,Bind_Rs).
lfp_query_(_Ps,_Is,Ts,Ts).


%!	tp_query(+Query,+Int_Acc,-Interpretation,+Res_Acc,-Result) is
%!	det.
%
%	TP operator for lfp_query_/4.
%
%	Differs from bog-standard TP operator implementation (as in
%	tp/4) in that it binds the Result of Query separatly from the
%	Interpretation (which is itself updated with any new
%	consequences of Query.
%
tp_query([],Is,Is,Ts,Ts):-
	!.
tp_query([C|Ps],Is_Acc,Is_Bind,Rs_Acc,Rs_Bind):-
	copy_term(C,C_)
	,clause_head_body(C_,H,B)
	,model_subset(B,Is_Acc)
	,\+ memberchk(H,Is_Acc)
	%,\+ memberchk(H,Rs_Acc) %?
	,!
	,tp_query(Ps,[H|Is_Acc],Is_Bind,[H|Rs_Acc],Rs_Bind).
tp_query([_C|Ps],Is_Acc,Is_Bind,Rs_Acc,Rs_Bind):-
	tp_query(Ps,Is_Acc,Is_Bind,Rs_Acc,Rs_Bind).



%!	lfp(+Program,-LFP) is det.
%
%	Calculates the Least Fixed Point of a Program.
%
%	The LFP of a Progam is its Least Herbrand Model.
%
lfp(Ps,Ts):-
	lfp(Ps,[],[],Ts).


%!	lftp(+Program,+Interpretation,+Acc,-Bind) is det.
%
%	Business end of lfp/2. Recursively calculates the Least Fixed
%	Point of a Program under an Interpretation. In each recursive
%	step the LFP of the program calculated in this step is added to
%	the current Interpretation, until the Interpretation stops
%	changing.
%
lfp(Ps,Is,Ts_i,Bind):-
	tp(Ps,Is,Ts_i,Ts_k)
	,Ts_i \= Ts_k
	,!
	,lfp(Ps,Ts_k,Ts_k,Bind).
lfp(_Ps,_Is,Ts,Ts).


%!	tp(+Program,+Interpretation,+Acc,-TP) is det.
%
%	Implements a TP operator.
%
%	The TP operator is an example of bottom-up programming. It
%	calculates the immediate consequences of a set of premises. In
%	logic programming, the premises are the Program and the
%	consequences are the set of atoms entailed by the Program.
%
%	This implementation employs a small optimisation in that an atom
%	is added to the accumulator of immediate consequences only if it
%	is not already in the accumulator. Apparently, there are still
%	implementations that can be applied beyond this.
%
tp([],_Is,Ts,Ts):-
	!.
tp([C|Ps],Is,Acc,Bind):-
	copy_term(C,C_)
	,clause_head_body(C_,H,B)
	,model_subset(B,Is)
	,\+ memberchk(H,Acc)
	,!
	,tp(Ps,Is,[H|Acc],Bind).
tp([_C|Ps],Is,Acc,Bind):-
	tp(Ps,Is,Acc,Bind).


%!	clause_head_body(+Clause,-Head,-Body) is nondet.
%
%	Head and Body literals of a Clause.
%
clause_head_body(H:-B,H,B).
clause_head_body(H,H,true):-
	H \= (_:-_).


%!	model_subset(+Literal,+Model) is det.
%
%	True if Literal is in Model
%
model_subset(true,_Ms):-
	!.
model_subset((L), Ms):-
% L is a single literal
	L \= (_,_)
	,!
	,member(L, Ms).
model_subset((L,Ls), Ms):-
% L is a set of literals
	L \= (_,_)
	,!
	,member(L, Ms)
	,model_subset(Ls,Ms).
model_subset((L,Ls), Ms):-
% L is a set of literals enclosed in parentheses.
	L = (_,_)
	,model_subset(L, Ms)
	,model_subset(Ls,Ms).
