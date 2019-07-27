:-module(mathemancy, [factorial/2
		     ,average/2
		     ,series/3
		     ,interval/4
		     ,pairs_sd/3
		     ,standard_deviation/3
		     ,pairs_averages/2
		     ,standard_error/3
		     ]).

:-use_module(library(clpfd), [transpose/2]).

/** <module> Statistics, probabilities and mathematics.
*/


%!	series(+Start,+End,-Series) is det.
%
%	Series is a list of numbers from Start to End.
%
%	Abstracts the useful boilerplate of generating a list of numbers
%	from Start to End using findall/3 and between/2.
%
series(I, K, Ss):-
	findall(J
	       ,between(I,K,J)
	       ,Ss).



%!	interval(+I,+J,+K,-Sequence) is det.
%
%	Generate a Sequence from I to J increasing by K.
%
%	Sequence is a list of numbers in the (closed) interval [I,J]
%	increasing with stride K.
%
%	Examples of use:
%	==
%	?- interval(0,10,1,_Ss), writeln(_Ss).
%	[0,1,2,3,4,5,6,7,8,9,10]
%	true.
%
%	?- interval(1,10,1,_Ss), writeln(_Ss).
%	[1,2,3,4,5,6,7,8,9,10]
%	true.
%
%	?- interval(2,10,2,_Ss), writeln(_Ss).
%	[2,4,6,8,10]
%	true.
%
%	?- interval(2,10,3,_Ss), writeln(_Ss).
%	[2,5,8]
%	true.
%
%	?- interval(1,1000,101,_Ss), writeln(_Ss).
%	[1,102,203,304,405,506,607,708,809,910]
%	true.
%
%	?- interval(1,1000,100,_Ss), writeln(_Ss).
%	[1,101,201,301,401,501,601,701,801,901]
%	true.
%
%	?- interval(1,10,11,_Ss), writeln(_Ss).
%	[1]
%	true.
%
%	?- interval(10,9,1,_Ss), writeln(_Ss).
%	[10]
%	true.
%	==
%
interval(I,J,K,Ss):-
	interval(I,J,K,[I],Ss_)
	,reverse(Ss_,Ss).


%!	interval(+I,+J,+K,+Acc,-Bind) is det.
%
%	Business end of interval/4.
%
%	Acc is seeded with the initial value of I. I is incremented
%	by K each turn and the result added to Acc, until I is equal to
%	or greater than J. When I is equal to J it is added to the Bind
%	list; when it's greater than J, it is discarded. Then Bind is
%	returned. interval/4 reverses Bind to get the sequence in the
%	right order.
%
interval(N,J,_K,Is,Is):-
	N >= J
	,!.
interval(I,J,K,Acc,Bind):-
	N is I + K
	,N =< J
	,!
	,interval(N,J,K,[N|Acc],Bind).
interval(_I,_J,_K,Is,Is).



%!	factorial(+N,-Factorial) is det.
%
%	Calculate the Factorial of N
%
factorial(0,1):-
	!.
factorial(N, F):-
	factorial(N, 1, F).


%!	factorial(+N,+F,-G) is det.
%
%	Business end of factorial/2.
%
factorial(1,F,F):-
	!.
factorial(N,F,G):-
	F_ is N * F
	,N_ is N - 1
	,factorial(N_,F_,G).



%!	average(+Numbers,-Mean) is det.
%
%	Compute the arithmetic Mean of a list of Numbers.
%
average([], 0):-
	!.
average(Ns,A):-
	average(Ns,0,0,A).

average([],I,Sum,Avg):-
	Avg is Sum / I.
average([N|Ns],I,Acc,Bind):-
	succ(I,I_)
	,Acc_ is Acc + N
	,average(Ns,I_,Acc_,Bind).



%!	standard_deviation(+Values,+Mean,-SD) is det.
%
%	Calculate the deviation from the Mean of a list of Values.
%
standard_deviation(Xs,M,SD):-
	length(Xs, N)
	,N_ is max(N - 1, 1)
	,findall(Y
	       ,(member(X,Xs)
		,Y is (X - M)^2 / N_
		)
	       ,Ys)
	,sumlist(Ys,S)
	,SD is sqrt(S).



%!	standard_error(+List,+Sd,-SEM) is det.
%
%	Calculate the standard error of the mean of a List of numbers.
%
standard_error(Ns,Sd,Se):-
	length(Ns, N)
	,Se is Sd / sqrt(N).



%!	pairs_averages(+Pairs,-Averages) is det.
%
%	Calculate the Average column values of a list of Pairs lists.
%
%	Pairs is a list of lists of key-value pairs, where each element
%	is a list of key-value pairs K-V and each value, V is a number.
%
%	Average is a list of the average values at the same position in
%	each list. This implies (sort of) that each list in Pairs must
%	be of the same length. This predicate uses clpfd:transpose/2 to
%	transpose pairs lists before averaging their values, and that
%	predicate expets a list of lists of the same length as input,
%	failing which it fails. The same goes for this predicate also.
%
pairs_averages(Ps,As):-
	findall(Vs
	       ,(member(P,Ps)
		,pairs_keys_values(P,_Ks,Vs)
		)
	       ,VS)
	,transpose(VS,VS_T)
	,findall(Av
		,(member(T, VS_T)
		 ,average(T,Av)
		 )
		,As).



%!	pairs_sd(+Pairs,+Means,-SDs) is det.
%
%	Calculate the standard deviation of a list of Pairs.
%
%	Pairs is a list of _lists_ of key-value pairs, where the
%	value of each key-value pair is a number.
%
%	Means is the column-wise average value of the values in each
%	pair, as calculated by pairs_averages/2.
%
%	SDs is the standard deviation of each "column" in the list of
%	lsits, i.e. the standard deviation of the values in the same
%	position in each list from the mean of the same.
%
pairs_sd(Ps,As,SDs):-
	findall(Vs
	       ,(member(P,Ps)
		,pairs_keys_values(P,_Ks,Vs)
		)
	       ,VS)
	,transpose(VS,VS_T)
	,findall(SD
		,(nth1(I,VS_T,Col)
		 ,nth1(I,As,Av)
		 ,standard_deviation(Col,Av,SD))
		,SDs).
