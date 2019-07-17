:-module(evaluation, [print_confusion_matrix/2
		     ,print_evaluation/2
		     ,evaluation/5]).

:-use_module(configuration).

/** <module> Evaluation metrics for experiment results.
*/


%!	format_confusion_matrix(+Target,+Result) is det.
%
%	Print a confusion matrix for a learning Result.
%
print_confusion_matrix(T,Rs):-
	evaluation(T,Rs,[P,N],[PP,NN,NP,PN],[ACC,ERR,FPR,FNR,TPR,_TNR,PRE,FSC])
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



%!	print_evaluation(+Target,-Results) is det.
%
%	Print a simple listing of the evaluation of Results.
%
print_evaluation(T,Rs):-
	configuration:decimal_places(P)
	,evaluation(T,Rs,[_P,_N],[_PP,_NN,_NP,_PN],[ACC,ERR,FPR,FNR,TPR,TNR,PRE,FSC])
	,format('ACC: ~*f~n',[P,ACC])
	,format('ERR: ~*f~n',[P,ERR])
	,format('FPR: ~*f~n',[P,FPR])
	,format('FNR: ~*f~n',[P,FNR])
	,format('PRE: ~*f~n',[P,PRE])
	,format('REC: ~*f~n',[P,TPR])
	,format('SPE: ~*f~n',[P,TNR])
	,format('FSC: ~*f~n',[P,FSC]).



%!	evaluation(+Target,+Results,-Totals,+Base,-Calculated) is det.
%
%	Evaluate a Result according to the learning Target.
%
evaluation(T,Rs,[P,N],[PP_,NN_,NP_,PN_],[ACC,ERR,FPR,FNR,TPR,TNR,PRE,FSC]):-
	experiment_data(T,Pos,Neg,_BK,_MS)
	,maplist(sort,[Rs,Pos,Neg],[Rs_,Pos_,Neg_])
	,length(Pos_,P)
	,length(Neg_,N)
	,true_positives(Rs_,Pos,PP)
	,true_negatives(Rs_,Neg,NN)
	,false_positives(Rs_,Neg,NP)
	,false_negatives(Rs_,Pos,PN)
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
	,FSC is 2 * (P / S).


%!	false_positives(+Results,+Negative,-NP) is det.
%
%	False positives in a result.
%
%	Under a closed world assumption an instance, X, is a false
%	positive iff X is in the intersection of the set of Negative
%	examples and the set of positive predicted Results.
%
false_positives(Rs,Neg,NP):-
	ord_intersection(Rs,Neg,NP).


%!	false_negaives(+Results,+Positive,-PN) is det.
%
%	False negatives in a result.
%
%	Under a closed world assumption an instance, X, is a false
%	negative iff X is in the set of Positive examples and X is not
%	in the set of positive predicted Results.
%
false_negatives(Rs,Pos,PN):-
	ord_subtract(Pos,Rs,PN).


%!	true_positives(+Results,+Positive,-PP) is det.
%
%	True positives in a result.
%
%	Under a closed world assumption an instance, X, is a true
%	positive iff X is in the intersection of the set of Positive
%	examples and the set of positive predicated Results.
%
true_positives(Rs,Pos,PP):-
	ord_intersection(Rs,Pos,PP).


%!	true_negatives(+Results,+Negative,-TN) is det.
%
%	True negatives in a result.
%
%	Under a closed world assumption an instance, X, is a true
%	negative iff X is in the set of Negative examples and X is not
%	in the set of positive reported Results.
%
true_negatives(Rs,Neg,NN):-
	ord_subtract(Neg,Rs,NN).


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
	,R is N / M.
