:-module(tp, [lfp_query/4
	     ,lfp_query/3
	     ,lfp/2]).

/** <Module> Bottom up evaluation of definite prorgams.
*/


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


%!	clause_head_body(+Clause,-Head,-Body) is det.
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
