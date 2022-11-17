:-module(evaluation, [train_and_test/5
		     ,train_and_test/6
		     ,timed_train_and_test/6
		     ,timed_train_and_test/7
		     ,print_evaluation/3
		     ,print_evaluation/7
		     ,list_results/3
		     ,list_results/6
		     ,print_metrics/2
		     ,print_metrics/5
		     ,false_positives/3
		     ,false_negatives/3
		     ,true_positives/3
		     ,true_negatives/3
		     ]).

:-use_module(project_root(configuration)).
:-use_module(src(auxiliaries)).

% Ensure the learner module is loaded.
% It should be located in project_root(src)
% and have a configuration option learner/1
% declaring its name.
:- configuration:learner(L)
  ,use_module(src(L)).

:-use_module(lib(sampling/sampling)).
:-use_module(lib(tp/tp)).


/** <module> Evaluation metrics for experiment results.
*/


%!	train_and_test(+Target,+Sample,-Program,+Metric,-Value) is det.
%
%	Learn a Program and evaluate it by the requested Metric.
%
%	This version obtains the MIL problem (examples, BK and
%	metarules) from the current experiment file.
%
%	Sample is the size of the training partition for each of the
%	positive and negative examples, as a float between 0 and 1.0 or
%	as an integer. The testing partition is the complement of Sample
%	with respect to the entire set of positive or negative examples.
%
%	Sample can be given as a compound S_Pos/S_Neg, where S_Pos is
%	the size of the training partition of the positive examples and
%	S_Neg the size of the training partition of the negative
%	examples.
%
%	Metric is one of: [acc, err, fpr, fnr, tpr, tnr, pre, fsc],
%	corresponding to the metrics calculated by evaluation/6.
%
%	Value is the value of the corresponding metric, computed by
%	evaluating Program on the testing partition selected according
%	to Sample.
%
train_and_test(T,S,Ps,M,V):-
	experiment_data(T,Pos,Neg,BK,MS)
	,train_and_test(T,S,[Pos,Neg,BK,MS],Ps,M,V).



%!	train_and_test(+Target,+Sample,+Problem,-Prog,+Metric,-Val)
%!	is det.
%
%	Learn a Program and evaluate the results by the given Metric.
%
%	Sample is the size of the training partition for each of the
%	positive and negative examples, as a float between 0 and 1.0 or
%	as an integer. The testing partition is the complement of Sample
%	with respect to the entire set of positive or negative examples.
%
%	Sample can be given as a compound S_Pos/S_Neg, where S_Pos is
%	the size of the training partition of the positive examples and
%	S_Neg the size of the training partition of the negative
%	examples.
%
%	Problem is a list [Pos, Neg, BK, MS] whose elements are the
%	elements of a MIL problem: example atoms, background knowledge
%	symbols and arities and metarule names of the MIL problem.
%
%	Prog is the program learned from the given Problem.
%
%	Metric is one of: [acc, err, fpr, fnr, tpr, tnr, pre, fsc],
%	corresponding to the metrics calculated by evaluation/6.
%
%	Val is the value of the corresponding metric, computed by
%	evaluating Prog on the testing partition selected according to
%	Sample.
%
train_and_test(T,S,[Pos,Neg,BK,MS],Ps,M,V):-
	data_partitions(S,Pos,Neg,Pos_Train,Neg_Train,Pos_Test,Neg_Test)
	,debug(evaluation, 'Learning program...', [])
	,learning_query(Pos_Train,Neg_Train,BK,MS,Ps)
	,debug(evaluation, 'Evaluating learned program...', [])
	,program_results(T,Ps,BK,Rs)
	,evaluation(Rs,Pos_Test,Neg_Test,_Ts,_Bs,Cs)
	,once(metric(M,Cs,V))
	,debug(evaluation, 'Completed evaluation. ~w: ~w', [M,V]).



%!	timed_train_and_test(+Target,+Sample,+Limit,-Program,+Metric,-Value)
%!	is det.
%
%	Learn a Program and evaluate it by the requested Metric.
%
%	As train_and_test/5 by additionally imposes a time Limit of that
%	many seconds to learning.
%
%	This version obtains the MIL problem (examples, BK and
%	metarules) from the current experiment file.
%
timed_train_and_test(T,S,L,Ps,M,V):-
	experiment_data(T,Pos,Neg,BK,MS)
	,timed_train_and_test(T,S,L,[Pos,Neg,BK,MS],Ps,M,V).



%!	timed_train_and_test(+Target,+Sample,+Limit,+Problem,-Prog,+Metric,-Val)
%!	is det.
%
%	Learn a program and evaluate the results by the given Metric.
%
%	As train_and_test/6 but additionally, a time Limit of that many
%	seconds is imposed on learning. If learning does not complete
%	(successfully or not) in Limit seconds, the empty hypothesis is
%	ealuated as the result of learning.
%
timed_train_and_test(T,S,L,[Pos,Neg,BK,MS],Ps,M,V):-
	debug(evaluation, 'Partitioning data...', [])
	,data_partitions(S,Pos,Neg,Pos_Train,Neg_Train,Pos_Test,Neg_Test)
	,G = learning_query(Pos_Train,Neg_Train,BK,MS,Ps)
	,C = call_with_time_limit(L,G)
	,debug(evaluation, 'Learning program...', [])
	,catch(C,time_limit_exceeded,(Ps=[]
				     ,cleanup_experiment
				     ,debug(evaluation,'Time limit exceeded',[])
				     )
	      )
	,debug(evaluation, 'Evaluating learned program...', [])
	,program_results(T,Ps,BK,Rs)
	,evaluation(Rs,Pos_Test,Neg_Test,_Ts,_Bs,Cs)
	,once(metric(M,Cs,V))
	,debug(evaluation, 'Completed evaluation. ~w: ~w', [M,V]).



%!	print_evaluation(+Target,+Sample,-Program) is det.
%
%	Print evaluation metrics of a learned Program.
%
%	Program is evaluated in the context of a MIL problem (examples
%	and background knowledge) obtained from the current experiment
%	file.
%
print_evaluation(T,S,Ps):-
	experiment_data(T,Pos,Neg,BK,MS)
	,print_evaluation(T,S,Pos,Neg,BK,MS,Ps).



%!	print_evaluation(+Target,+Sample,+Pos,+Neg,+BK,+Metarules,-Program)
%	is det.
%
%	Print evaluation metrics of a learned Program.
%
print_evaluation(T,S,Pos,Neg,BK,MS,Ps):-
	data_partitions(S,Pos,Neg,Pos_Train,Neg_Train,Pos_Test,Neg_Test)
	,learning_query(Pos_Train,Neg_Train,BK,MS,Ps)
	,print_evaluation(T,Ps,Pos_Test,Neg_Test,BK).


%!	print_evaluation(+Target,+Program,+Pos,+Neg,+BK) is det.
%
%	Print evaluation metrics of a learned Program.
%
print_evaluation(T,Ps,Pos,Neg,BK):-
	program_results(T,Ps,BK,Rs)
	,clause_count(Ps,N,D,U)
	,nl
	,format('Hypothesis size:  ~w~n',[N])
	,format('Definite clauses: ~w~n',[D])
	,format('Unit clauses:	  ~w~n',[U])
	,nl
	,print_confusion_matrix(Rs,Pos,Neg).



%!	data_partitions(+Size,+Pos,+Neg,-Pos_Tr,-Neg_Tr,-Pos_Ts,-Neg_Ts)
%!	is det.
%
%	Split data into training and testing partitions.
%
%	Helper to alow sampling Size to be given as a term S_Pos/S_Neg,
%	without too much code duplication.
%
%	Size is the sampling size given as an integer, or a float, or a
%	pair S_Pos/S_Neg, of integers or floats (or a mix thereof,
%	currently).
%
%	Pos, Neg are the initial sets of positive and negative examples.
%
%	Pos_Tr, Neg_Tr are the training partitions of the positive and
%	negative examples, respectively, sampled up to Size from Pos and
%	Neg.
%
%	Pos_Ts and Neg_Ts are the testing partitions, containing the
%	positive and negative examples, respectively, remaining after
%	the selection of the positive partitions.
%
data_partitions(S,Pos,Neg,Pos_Train,Neg_Train,Pos_Test,Neg_Test):-
	(   S = S_Pos/S_Neg
	 ->  true
	 ;   S = S_Pos
	    ,S = S_Neg
	 )
	,train_test_splits(S_Pos,Pos,Pos_Train,Pos_Test)
	,train_test_splits(S_Neg,Neg,Neg_Train,Neg_Test).


%!	train_test_splits(+Size,+Examples,-Training,-Testing) is det.
%
%	Split a set of Examples to Training and Testing partitions.
%
%	Raises error if Size is equal to 1.0 or 0.0.
%
train_test_splits(P,[],[],[]):-
% There may be no negative examples.
% But this better not be the positive examples.
	(   P > 0
	->  throw('The size of an empty partition must be equal to 0!')
	;   true
	)
	,!.
train_test_splits(P,_Es,_Train,_Test):-
	float(P)
	,P >= 1.0
	,throw('The size of the testing partition must be more than 0!').
train_test_splits(P,_Es,_Train,_Test):-
	P =< 0.0
	,throw('The size of the training partition must be more than 0!').
train_test_splits(_P,[],[],[]):-
% An example set may be empty- but it better be the negative examples!
	!.
train_test_splits(P,Es,Train,Test):-
	float(P)
	,!
	,p_list_partitions(P,Es,Train,Test).
train_test_splits(K,Es,Train,Test):-
	integer(K)
	,k_list_partitions(K,Es,Train,Test).


%!	metric(?Metric,?Metrics,?Value) is det.
%
%	Obtain a Value for the given Metric in a list of Metrics.
%
%	Metrics is the last argument of evaluation/6. Metric is one of:
%	[acc, err, fpr, fnr, tpr, tnr, pre, fsc]. Value is the value in
%	Metrics corresponding to Metric.
%
metric(acc,[ACC,_ERR,_FPR,_FNR,_TPR,_TNR,_PRE,_FSC],ACC).
metric(err,[_ACC,ERR,_FPR,_FNR,_TPR,_TNR,_PRE,_FSC],ERR).
metric(fpr,[_ACC,_ERR,FPR,_FNR,_TPR,_TNR,_PRE,_FSC],FPR).
metric(fnr,[_ACC,_ERR,_FPR,FNR,_TPR,_TNR,_PRE,_FSC],FNR).
metric(tpr,[_ACC,_ERR,_FPR,_FNR,TPR,_TNR,_PRE,_FSC],TPR).
metric(tnr,[_ACC,_ERR,_FPR,_FNR,_TPR,TNR,_PRE,_FSC],TNR).
metric(pre,[_ACC,_ERR,_FPR,_FNR,_TPR,_TNR,PRE,_FSC],PRE).
metric(fsc,[_ACC,_ERR,_FPR,_FNR,_TPR,_TNR,_PRE,FSC],FSC).



%!	list_results(+Target,+Program,+Results) is det.
%
%	List True/False Positives/Negative atoms of Program.
%
%	Results is either a list of one or more of the constants
%	[pp,nn,np,pn], or the constant "all". Their meaning is as
%	follows:
%	* pp: Print true positive atoms
%	* nn: Print true negative atoms
%	* np: Print false positive atoms
%	* pn: Print false negative atoms
%	* all: Print all atoms.
%
list_results(T,Ps,Rs):-
	experiment_data(T,Pos,Neg,BK,_MS)
	,list_results(T,Ps,Pos,Neg,BK,Rs).

%!	list_results(+Target,+Program,+Pos,+Neg,+BK,+Results) is det.
%
%	Business end of list_results/3.
%
list_results(T,Ps,Pos,Neg,BK,Rs):-
	convert_examples(Pos,Neg,Pos_c,Neg_c)
	,program_results(T,Ps,BK,As)
	,maplist(sort,[As,Pos_c,Neg_c],[As_,Pos_,Neg_])
	,false_positives(As_,Neg_,NP)
	,length(NP,NP_n)
	,false_negatives(As_,Pos_,PN)
	,length(PN,PN_n)
	,true_positives(As_,Pos_,PP)
	,length(PP,PP_n)
	,true_negatives(As_,Neg_,NN)
	,length(NN,NN_n)
	,(   Rs == all
	 ->  Rs_ = [pp,nn,np,pn]
	 ;   Rs_ = Rs
	 )
	% Yeah, I know.
	,(   memberchk(pp,Rs_)
	 ->  format('\nTrue positives: ~w~n',[PP_n])
	    ,print_clauses(PP)
	 ;   true
	 )
	,(   memberchk(nn,Rs_)
	 ->  format('\nTrue negatives: ~w~n',[NN_n])
	    ,print_clauses(NN)
	 ;   true
	 )
	,(   memberchk(np,Rs_)
	 ->  format('\nFalse positives: ~w~n',[NP_n])
	    ,print_clauses(NP)
	 ;   true
	 )
	,(   memberchk(pn,Rs_)
	 ->  format('\nFalse negatives: ~w~n',[PN_n])
	    ,print_clauses(PN)
	 ;   true
	 ).


%!	program_results(+Target,+Program,+BK,-Results) is det.
%
%	Collect Results of a learned Program.
%
%	Program is a learned hypothesis. Results is a list of atoms that
%	are immediate consequences of the Program with respect to the
%	background knowledge, BK.
%
program_results(_T,[],_BK,[]):-
	!.
program_results(T,Ps,BK,Rs):-
	configuration:success_set_generation(tp)
	,!
	,ground_background(T,BK,BK_)
	,lfp_query(Ps,BK_,_Is,Rs).
program_results(F/A,Ps,_BK,Rs):-
	configuration:success_set_generation(sld)
	%,configuration:experiment_file(_P, M)
	,manage_residue(F/A,Ps,Ps_)
	,S = (table(program_results:F/A)
	     ,assert_program(program_results,Ps_,Refs_Ps)
	     % Ensure BK definitions are visible to program_results.
	     ,add_import_module(program_results,experiment_file,start)
	     )
	,G = (findall(H
		     ,(functor(H,F,A)
		      ,call(program_results:H)
		      )
		     ,Rs_)
	     ,untable(program_results:F/A)
	     ,predsort(unifiable_compare,Rs_, Rs)
	     )
	,C = (erase_program_clauses(Refs_Ps)
	     ,untable(program_results:F/A)
	     ,delete_import_module(program_results,experiment_file)
	     )
	,setup_call_cleanup(S,G,C).



%!	manage_residue(+Target,+Program,-Managed) is det.
%
%	Keep or discard atomic residue in a Program.
%
%	Selects a subset of Program according to the value of
%	evaluate_atomic_residue/1 that determines whether atomic residue
%	is to be included, excluded or isolated from the learned Program
%	before measure accuracy. For example, we might want to know what
%	is the contribution to accuracy of non-atomic clauses in
%	Program (option "exclude"), or, conversely, what is the
%	contribution to accuracy of atomic residue (option "isolate").
%
%	@see evaluate_atomic_residue/1.
%
manage_residue(F/A,Ps,Ps_):-
	configuration:evaluate_atomic_residue(R)
	,(   R = include
	 ->  Ps_ = Ps
	 ;   R = exclude
	 ->  findall(H:-B
		    ,member(H:-B,Ps)
		    ,Ps_)
	 ;   R = isolate
	 ->  findall(C
		    ,(member(C,Ps)
		     ,functor(C,F,A)
		     )
		    ,Ps_)
	 ;   format(atom(E),'evaluate_atomic_residue/1: unknown option ~w',[R])
	    ,throw(E)
	 ).


%!	ground_background(+Target,+BK,-Ground) is det.
%
%	Collect ground BK atoms.
%
%	Also remove from the BK atoms of the learning Target. That's to
%	allow lfp/2 to succeed if the learning Target is also a
%	predicate in the BK (more precisely, if it is a determinant of
%	another BK predicate).
%
ground_background(F/A,BK,BK_):-
	closure(BK,user,Cs)
	,flatten(Cs, Ps)
	,lfp(Ps,As)
	,findall(At
		,(member(At,As)
		 ,\+ functor(At,F,A)
		 )
		,BK_).


%!	clause_count(+Hypothesis,-Size,-Definite,-Unit) is det.
%
%	Count the clauses in a hypothesis.
%
%	Size is the number of all clauses in Hypothesis. Definite is the
%	number of Definite clauses in Hypothesis and Unit the number of
%	unit clauses in Hypothesis.
%
clause_count(Ps,N,D,U):-
	findall(H:-B
	       ,member(H:-B,Ps)
	       ,Ds)
	,length(Ps,N)
	,length(Ds,D)
	,U is N - D.


%!	print_confusion_matrix(+Results,+Pos,+Neg) is det.
%
%	Print a confusion matrix for a set of learning Results.
%
print_confusion_matrix(Rs,Pos,Neg):-
	evaluation(Rs,Pos,Neg
		  ,[P,N],[PP,NN,NP,PN],[ACC,ERR,FPR,FNR,TPR,_TNR,PRE,FSC])
	,PPNP is PP + NP
	,PNNN is PN + NN
	,S is P + N
	,format_confusion_matrix([PP,PN,NP,NN]
				,[P,N,PPNP,PNNN,S]
				,[ACC,ERR,FPR,FNR,PRE,TPR,FSC]
				).


%!	format_confusion_matrix(+Counts,+Totals,+Metrics) is det.
%
%	Bussiness end of print_confusion_matrix/2.
%
%	Actually prints a confusion matrix for a result.
%
%	Counts is a list of numbers [PP,PN,NP,NN,T], where:
%	* PP: positivie instances predicted as true
%	* PN: positive instances predicted as negative
%	* NP: negative instances predicted as positive
%	* NN: negative instances predicted as negative
%	* T:  total positive and negatives predicted
%
%	Totals is a list of numbers [TP,TN,PPNP,PNNN,T], where:
%	* TP: the total number of positive instances
%	* TN: the total number of negative instances
%	* PPNP: the sum of PP + NP
%	* PNNN: the sum of PN + NN
%
%	Metrics is a list of numbers [Acr,Err,FPR,FNR,PRE,REC,FSC],
%	where:
%	* Acr: Accuracy, calculated as  PP + NN / T
%	* Err: Error, calculated as NP + PN / T
%	* FPR: False Positive Rate, NP / TN
%	* FNR: False Negative Rate, PN / TP
%	* PRE: Precision, calculated as PP / PPNP
%	* REC: Recall, calculated as PP / TP (i.e. TPR)
%	* FSC: F-Score, PRE * REC / PRE + REC
%
%	Given the above lists of numbers, format_confusion_matrix/3 will
%	print approximately the following table (with some differences
%	in formatting):
%
%			Predicted +	Predicted -	Total
%	Actual +	PP		PN		TP
%	Actual -	NP		NN		TN
%	-----------------------------------------------------
%	Total		PPNP		PNNN		T
%
%	Accuracy:		PP + NN / T
%	Error:			NP + PN / T
%	False Positive Rate:	NP / TN
%	False Negative Rate:	PN / TP
%	Precision:		PP / PPNP
%	Recall(TPR):		PP / TP
%	F-Score:                Precision * Recall / Precision + Recall
%
format_confusion_matrix([PP,PN,NP,NN]
		       ,[TP,TN,PPNP,PNNN,T]
		       ,[ACC,ERR,FPR,FNR,PRE,REC,FSC]):-
	configuration:decimal_places(D)
	,atom_chars('Actual + Predicted + Predicted - Total', Hs)
	% Length of an entire header line
	,length(Hs, L1)
	,atom_chars('Actual + ', Act)
	% Length of second line's first column
	,length(Act, L21)
	,atom_chars('Predicted + ', Pred_p)
	,length(Pred_p, L22)
	,atom_chars('Predicted - ', Pred_n)
	,length(Pred_n, L23)
	% Printing header line
	,format('~*+~w ~*+~w ~*+~w~*+~n'
	       ,[L21,'Predicted +',L22,'Predicted -',L23,'Total',L1])
	,format('~w ~*+~w ~*+~w ~*+~w~n'
	       ,['Actual +',L21,PP,L22,PN,L23,TP])
	,format('~w ~*+~w ~*+~w ~*+~w~n'
	       ,['Actual -',L21,NP,L22,NN,L23,TN])
	,format('-------------------------------------~n',[])
	,format('~w ~*+~w ~*+~w ~*+~w~n'
	       ,['Total',L21,PPNP,L22,PNNN,L23,T])
	% Longest left column
	,atom_chars('False Positive Rate: ',TPR_cs)
	,length(TPR_cs, TPR_cs_L)
	,nl
	,format('Accuracy: ~*+~*f~n', [TPR_cs_L,D,ACC])
	,format('Error: ~*+~*f~n', [TPR_cs_L,D,ERR])
	,format('False Positive Rate: ~*+~*f~n', [TPR_cs_L,D,FPR])
	,format('False Negative Rate: ~*+~*f~n', [TPR_cs_L,D,FNR])
	,format('Precision: ~*+~*f~n', [TPR_cs_L,D,PRE])
	,format('Recall (TPR): ~*+~*f~n', [TPR_cs_L,D,REC])
	,format('F-Score: ~*+~*f~n', [TPR_cs_L,D,FSC]).



%!	print_metrics(+Target,+Program) is det.
%
%	Print a simple listing of evaluation metrics.
%
%	Program is evaluated in the context of a MIL problem (examples
%	and BK) obtained from the current experiment file.
%
print_metrics(T,Ps):-
	experiment_data(T,Pos,Neg,BK,_MS)
	,print_metrics(T,Ps,Pos,Neg,BK).



%!	print_metrics(+Target,+Program,+Pos,+Neg,+BK) is det.
%
%	Print a simple listing of evaluation metrics.
%
print_metrics(T,Ps,Pos,Neg,BK):-
	configuration:decimal_places(P)
	,program_results(T,Ps,BK,Rs)
	,evaluation(Rs,Pos,Neg
		   ,[_P,_N],[_PP,_NN,_NP,_PN],[ACC,ERR,FPR,FNR,TPR,TNR,PRE,FSC])
	,format('ACC: ~*f~n',[P,ACC])
	,format('ERR: ~*f~n',[P,ERR])
	,format('FPR: ~*f~n',[P,FPR])
	,format('FNR: ~*f~n',[P,FNR])
	,format('PRE: ~*f~n',[P,PRE])
	,format('REC: ~*f~n',[P,TPR])
	,format('SPE: ~*f~n',[P,TNR])
	,format('FSC: ~*f~n',[P,FSC]).



%!	evaluation(+Results,+Pos,+Neg,-Totals,+Base,-Calculated)
%!	is det.
%
%	Business end of evaluation/5
%
%	Evaluate a set of Results according to the Positive and Negative
%	examplse of the learning Target.
%
evaluation(Rs,Pos,Neg
	  ,[P,N],[PP_,NN_,NP_,PN_],[ACC,ERR,FPR,FNR,TPR,TNR,PRE,FSC]):-
	%maplist(print_clauses,['Results','Examples'],[Rs,Pos]),
	convert_examples(Pos,Neg,Pos_c,Neg_c)
	,maplist(sort,[Rs,Pos_c,Neg_c],[Rs_,Pos_,Neg_])
	,maplist(length,[Pos_,Neg_],[P,N])
	,true_positives(Rs_,Pos_,PP)
	,true_negatives(Rs_,Neg_,NN)
	,false_positives(Rs_,Neg_,NP)
	,false_negatives(Rs_,Pos_,PN)
	,maplist(length,[PP,NN,NP,PN],[PP_,NN_,NP_,PN_])
	,acc(PP,NN,Pos_,Neg_,ACC)
	,err(ACC,ERR)
	,tpr(PP,Pos_,TPR)
	,tnr(NN,Neg_,TNR)
	,fnr(PN,Pos_,FNR)
	,fpr(NP,Neg_,FPR)
	,pre(Rs_,PP,PRE)
	,fsc(PRE,TPR,FSC).


%!	acc(+PP,+NN,+Pos,+Neg,-ACC) is det.
%
%	Calculate the Accuracy of a result.
%
acc(_PP,_NN,[],[],_ACC):-
	throw('acc/5: Empty testing partition. Cannot evaluate hypothesis.').
acc(PP,NN,Pos,Neg,ACC):-
	total(PP,NN,Ps)
	,total(Pos,Neg,As)
	,ACC is Ps / As.

%!	err(+ACC,-ERR) is det.
%
%	Calculate the Error of a result.
%
err(ACC,Er):-
	Er is 1	- ACC.

%!	tpr(+PP,+Pos,+TPR) is det.
%
%	Calculate the True Positive Rate of a result.
%
tpr(PP,Pos,TPR):-
	ratio(PP,Pos,TPR).

%!	tpr(+NN,+Neg,+TNR) is det.
%
%	Calculate the True Negative Rate of a result.
%
tnr(NN,Neg,TNR):-
	ratio(NN,Neg,TNR).

%!	tpr(+PN,+Pos,+FNR) is det.
%
%	Calculate the False Negative Rate of a result.
%
fnr(PN,Pos,FNR):-
	ratio(PN,Pos,FNR).

%!	tpr(+NP,+NEG,+FPR) is det.
%
%	Calculate the False Positive Rate of a result.
%
fpr(NP,Neg,FPR):-
	ratio(NP,Neg,FPR).

%!	pre(+Results,+PP,+PRE) is det.
%
%	Calculate the Precision metric of a result.
%
pre(Rs,PP,PRE):-
	ratio(PP,Rs,PRE).

%!	fsc(+PRE,+REC,+FSC) is det.
%
%	Calculate the F1-score metric of a result.
%
fsc(PRE,REC,FSC):-
	P is PRE * REC
	,S is PRE + REC
	,safe_division(P,S,D)
	,FSC is 2 * D.


%!	false_positives(+Results,+Negative,-NP) is det.
%
%	False positives in a result.
%
%	Under a closed world assumption an instance, X, is a false
%	positive iff X is in the intersection of the set of Negative
%	examples and the set of positive predicted Results.
%
false_positives(Rs,Neg,NP):-
	my_ord_intersection(Rs,Neg,NP).


%!	false_negaives(+Results,+Positive,-PN) is det.
%
%	False negatives in a result.
%
%	Under a closed world assumption an instance, X, is a false
%	negative iff X is in the set of Positive examples and X is not
%	in the set of positive predicted Results.
%
false_negatives(Rs,Pos,PN):-
	my_ord_subtract(Pos,Rs,PN).


%!	true_positives(+Results,+Positive,-PP) is det.
%
%	True positives in a result.
%
%	Under a closed world assumption an instance, X, is a true
%	positive iff X is in the intersection of the set of Positive
%	examples and the set of positive predicated Results.
%
true_positives(Rs,Pos,PP):-
	my_ord_intersection(Rs,Pos,PP).


%!	true_negatives(+Results,+Negative,-TN) is det.
%
%	True negatives in a result.
%
%	Under a closed world assumption an instance, X, is a true
%	negative iff X is in the set of Negative examples and X is not
%	in the set of positive reported Results.
%
true_negatives(Rs,Neg,NN):-
	my_ord_subtract(Neg,Rs,NN).


%!	difference(+Xs,+Ys,-Difference) is det.
%
%	Difference of the lengths of two lists.
%
difference(Xs,Ys,D):-
	length(Xs,N)
	,length(Ys,M)
	,D is N	- M.


%!	total(+Xs,+Ys,-Sum) is det.
%
%	Sum of the lengths of two lists.
%
total(Xs,Ys,S):-
	length(Xs,N)
	,length(Ys,M)
	,S is N + M.


%!	ratio(+Xs,+Ys,-Ratio) is det.
%
%	Ratio of the lengths of two lists.
%
ratio(Xs,Ys,R):-
	length(Xs,N)
	,length(Ys,M)
	,safe_division(N,M,R).


%!	safe_division(+A,+B,-C) is det.
%
%	Avoid dividing by zero.
%
%	If the denominator of a division is 0, return A, else divide
%	A/B and return the result in C.
%
safe_division(A,0,A):-
	!.
safe_division(A,B,C):-
	C is A / B.


% ================================================================================
%  Custom set operations
% ================================================================================
% Ordered set intersection and difference copied from library(ordsets)
% and adjusted to use unification as equality.


%!	my_ord_intersection(+Set1, +Set2, -Intersection) is det.
%
%	Intersection holds the common elements of Set1 and Set2.  Uses
%	ord_disjoint/2 if Intersection is bound to `[]` on entry.
%
%	@tbd Copied from library(ordsets), renamed and changed to use
%	unifiable_compare/3 instead of compare/3.
%
my_ord_intersection(Set1, Set2, Intersection) :-
	(   Intersection == []
	->  my_ord_disjoint(Set1, Set2)
	;   my_oset_int(Set1, Set2, Intersection)
	).


%!	ord_disjoint(+Set1, +Set2) is semidet.
%
%	True if Set1 and Set2  have  no   common  elements.  This is the
%	negation of ord_intersect/2.
%
%	@tbd Copied from library(ordsets).
%
my_ord_disjoint(Set1, Set2) :-
	\+ my_ord_intersect(Set1, Set2).

%!	my_ord_intersect(+Set1, +Set2) is semidet.
%
%	True if both ordered sets have a non-empty intersection.
%
%	@tbd Copied from library(ordsets).
%
my_ord_intersect([H1|T1], L2) :-
	my_ord_intersect_(L2, H1, T1).
my_ord_intersect_([H2|T2], H1, T1) :-
	unifiable_compare(Order, H1, H2),
	my_ord_intersect__(Order, H1, T1, H2, T2).
my_ord_intersect__(<, _H1, T1,  H2, T2) :-
	my_ord_intersect_(T1, H2, T2).
my_ord_intersect__(=, _H1, _T1, _H2, _T2).
my_ord_intersect__(>, H1, T1,  _H2, T2) :-
	my_ord_intersect_(T2, H1, T1).


%!	oset_int(+OSet1, +OSet2, -Int) is det.
%
%	ordered set intersection
%
%	@tbd Copied from library(oset).
%
my_oset_int([], _Int, []).
my_oset_int([H1|T1], L2, Int) :-
	my_isect2(L2, H1, T1, Int).

my_isect2([], _H1, _T1, []).
my_isect2([H2|T2], H1, T1, Int) :-
	unifiable_compare(Order, H1, H2),
	my_isect3(Order, H1, T1, H2, T2, Int).

my_isect3(<, _H1, T1,  H2, T2, Int) :-
	my_isect2(T1, H2, T2, Int).
my_isect3(=, H1, T1, _H2, T2, [H1|Int]) :-
	my_oset_int(T1, T2, Int).
my_isect3(>, H1, T1,  _H2, T2, Int) :-
	my_isect2(T2, H1, T1, Int).



%!	ord_subtract(+InOSet, +NotInOSet, -Diff) is det.
%
%	Diff is the set holding all elements of InOSet that are not in
%	NotInOSet.
%
%	@tbd Copied from library(ordsets).
%
my_ord_subtract(InOSet, NotInOSet, Diff) :-
	my_oset_diff(InOSet, NotInOSet, Diff).


my_oset_diff([], _Not, []).
my_oset_diff([H1|T1], L2, Diff) :-
	my_diff21(L2, H1, T1, Diff).

my_diff21([], H1, T1, [H1|T1]).
my_diff21([H2|T2], H1, T1, Diff) :-
	unifiable_compare(Order, H1, H2),
	my_diff3(Order, H1, T1, H2, T2, Diff).

my_diff12([], _H2, _T2, []).
my_diff12([H1|T1], H2, T2, Diff) :-
	unifiable_compare(Order, H1, H2),
	my_diff3(Order, H1, T1, H2, T2, Diff).

my_diff3(<,  H1, T1,  H2, T2, [H1|Diff]) :-
	my_diff12(T1, H2, T2, Diff).
my_diff3(=, _H1, T1, _H2, T2, Diff) :-
	my_oset_diff(T1, T2, Diff).
my_diff3(>,  H1, T1, _H2, T2, Diff) :-
	my_diff21(T2, H1, T1, Diff).
