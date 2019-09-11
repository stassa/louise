:-module(auxiliaries, [learning_targets/1
		      ,known_metarules/1
		      ,list_top_program_reduction/1
		      ,list_top_program/1
		      ,list_top_program/2
		      ,list_encapsulated_problem/1
		      ,list_mil_problem/1
		      ,initialise_experiment/0
		      ,cleanup_experiment/0
		      ,assert_program/3
		      ,erase_program_clauses/1
		      ,experiment_data/5
		      ,print_clauses/1
		      ,debug_clauses/2
		      ,program/3
		      ,closure/3
		      ]).

:-user:use_module(lib(term_utilities/term_utilities)).
:-user:use_module(lib(program_reduction/program_reduction)).
:-user:use_module(lib(mathemancy/mathemancy)).
:-use_module(src(mil_problem)).


%!	learning_targets(+Targets) is det.
%
%	Collect learning Targets defined in an experiment file.
%
%	Targets is the list of predicate symbols and arities of each of
%	the target predicates that have background knowledge
%	declarations in background/2 clauses in the current experiment
%	file.
%
learning_targets(Ts):-
	initialise_experiment
	,experiment_file(_P, M)
	,findall(T
		,M:background_knowledge(T, _BK)
		,Ts).



%!	known_metarules(-Ids) is det.
%
%	Collect the Ids of metarules known to the system.
%
known_metarules(Ids):-
	findall(Id
	       ,(configuration:current_predicate(metarule, H)
		,H =.. [metarule,Id|_]
		,clause(H,_B)
		)
	       ,Ids).



%!	list_top_program_reduction(+Target) is det.
%
%	List the top-program reduction step for a learning Target.
%
%	@tbd: This does no feedbacksies. So you only see the first step
%	of the reduction.
%
list_top_program_reduction(T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_,Ss])
	,top_program(Pos_,Neg_,BK_,MS_,Ss,Ms)
	,flatten([Ss,Pos_,BK_,Ms,MS_],Ps_)
	,reduction_report(Ps_).



%!	list_top_program(+Target) is det.
%
%	Pretty-print the Top program for a Target predicate.
%
%	Same as list_top_program(Target, true).
%
list_top_program(T):-
	list_top_program(T,true).



%!	list_top_program(+Target, +Unfold) is det.
%
%	Pretty-print the Top program for a Target predicate.
%
%	Unfold is one of [true,false]. If true, the Top program is
%	unfolded into a list of definite clauses before printing.
%	Otherwise it is printed as a list of metasubstitutions.
%
list_top_program(T,U):-
	experiment_data(T,Pos,Neg,BK,MS)
	,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_,Ss])
	,louise:write_program(Pos_,BK_,MS_,Ss,Refs)
	,louise:generalise(Pos_,MS_,Ss_Pos)
	,write_and_count('Generalisation:',Ss_Pos,U)
	,louise:specialise(Ss_Pos,Neg_,Ss_Neg)
	,nl
	,write_and_count('Specialisation:',Ss_Neg,U)
	,erase_program_clauses(Refs).


%!	write_and_count(+Message,+Metasubs,+Unfold) is det.
%
%	Auxiliary to list_top_program/2.
%
%	Pretty-print a set of Metasubs, its cardinality and a Message.
%	Unfold is a boolean inherited from list_top_program/2,
%	determining whether the Top program is unfolded to a list of
%	definite clauses before printing.
%
write_and_count(Msg,Cs,U):-
	(   U = true
	->  louise:unfolded_metasubs(Cs, Cs_)
	;   U = false
	->  Cs_ = Cs
	% Else fail silently to flumox the user. Nyahahaha!
	)
	,length(Cs_, N)
	,format_underlined(Msg)
	,print_clauses(Cs_)
	,format('Length:~w~n',[N]).



%!	list_encapsulated_problem(+Target) is det.
%
%	Pretty-print the encapsulation of a MIL problem.
%
%	Target is the symbol and arity of the target predicate in the
%	encapsulated MIL problem to be listed.
%
list_encapsulated_problem(T):-
	configuration:extend_metarules(E)
	,experiment_data(T,Pos,Neg,BK,MS)
	,predicate_signature(Pos,BK,Ss)
	,format_underlined('Predicate signature')
	,print_clauses(Ss)
	,nl
	,encapsulated_clauses(Pos,Pos_)
	,format_underlined('Positive examples')
	,print_clauses(Pos_)
	,nl
	,encapsulated_clauses(Neg,Neg_)
	,format_underlined('Negative examples')
	,print_clauses(Neg_)
	,nl
	,encapsulated_bk(BK,BK_)
	,(   E == true
	 ->  extended_metarules(MS,MS_)
	 ;   expanded_metarules(MS,MS_)
	 )
	,format_underlined('Background knowledge')
	,forall(member(P,BK_)
	       ,print_clauses(P)
	       )
	,nl
	,format_underlined('Metarules')
	,forall(member(M,MS_)
	       ,(print_clauses(M)
		)
	       ).



%!	list_mil_problem(+Target) is det.
%
%	List the elements of a MIL problem.
%
%	Target is the symbol and arity of the target predicate in the
%	MIL problem to be listed.
%
list_mil_problem(T):-
	experiment_data(T,Pos,Neg,BK,MS)
	,format_underlined('Positive examples')
	,print_clauses(Pos)
	,nl
	,format_underlined('Negative examples')
	,print_clauses(Neg)
	,nl
	,format_underlined('Background knowledge')
	,forall(member(P,BK)
	       ,(program(P,user,Ps)
		,format('~w:~n',[P])
		,print_clauses(Ps)
		,format('~n',[])
		)
	       )
	,format_underlined('Metarules')
	,forall(member(Id,MS)
	       ,(configuration:current_predicate(metarule,H)
		,H =.. [metarule,Id|_]
		,clause(H, B)
		,print_clauses([H:-B])
		)
	       ).


%!	format_underlined(+Atom) is det.
%
%	Print an atom and underline it.
%
format_underlined(A):-
	atom_underline(A,A_)
	,format('~w~n',[A])
	,format('~w~n',[A_]).


%!	atom_underline(+Atom,-Underlined) is det.
%
%	Create an Underline for an Atom.
%
atom_underline(A,A_):-
	atom_length(A, N)
	,findall(-
		,between(1,N,_)
		,Ds)
	,atomic_list_concat(Ds,A_).



%!	initialise_experiment is det.
%
%	Load and initialise the current experiment file.
%
initialise_experiment:-
	configuration:experiment_file(P,_M)
	,user:use_module(P).



%!	cleanup_experiment is det.
%
%	Clean up after a learning session.
%
%	Currently this only removes clauses of m/n asserted to the
%	dynamic database.
%
%	Remember to run initialise_experiment/0 after this one to
%	re-load any necessary clauses.
%
cleanup_experiment:-
	forall(user:current_predicate(m,H)
	      ,(user:retractall(H)
	       % Clauses in program module are asserted
	       % by predicates in program_reduction module
	       ,program:retractall(H)
	       )
	      ).



%!	assert_program(+Module,+Program,-Clause_References) is det.
%
%	As assert_program/2 but also binds a list of Clause_References.
%
assert_program(M,Ps,Rs):-
	assert_program(M,Ps,[],Rs).

assert_program(_,[],Rs,Rs):-
	!.
assert_program(M,[A|P],Acc,Bind):-
	clause(M:A,true)
	,!
	,assert_program(M,P,Acc,Bind).
assert_program(M,[H:-B|P],Acc,Bind):-
	clause(M:H,B)
	,!
	,assert_program(M,P,Acc,Bind).
assert_program(M,[C|P],Acc,Bind):-
	assert(M:C,Ref)
	,assert_program(M,P,[Ref|Acc],Bind).



%!	erase_program_clauses(-Clause_References) is det.
%
%	Erase a list of Clause_References from the dynamic database.
%
%	Clause_References is meant to be a list of references of a
%	program's clauses asserted to the dynamic database with
%	assert_program/3.
%
%	The purpose of this predicate is, very specifically, to allow a
%	learned theory previously asserted by invoking assert_program/3
%	during disprove/2, to be removed from the dynamic database
%	without stumbling over module scoping that can be complicated
%	when a predicate is declared in one module and then clauses of
%	it are added in another module.
%
%	For example, the following is what you should expect to see in
%	the dynamic database after a theory of father/2 is learned and
%	asserted in the dynamic database, while there is also background
%	knowledge of father/2:
%
%	==
%	[debug] [1]  ?- listing(thelma:father/2).
%	:- dynamic tiny_kinship:father/2.
%
%	tiny_kinship:father(stathis, kostas).
%	tiny_kinship:father(stefanos, dora).
%	tiny_kinship:father(kostas, stassa).
%	tiny_kinship:father(A, C) :-
%	    thelma:
%	    (   father_1(A, B),
%	        parent(B, C)
%	    ).
%
%	true.
%	==
%
%	This happens because we allow the same experiment modules to
%	export background predicates that have the same symbol and
%	arities with target predicates declared in the same module. It
%	means that it's very fiddly to remove the clauses of the learned
%	theory, especially while leaving the background predicate
%	untouched.
%
erase_program_clauses([]):-
	!.
erase_program_clauses([Ref|Rs]):-
	erase(Ref)
	,erase_program_clauses(Rs).



%!	experiment_data(+Target,-Positive,-Negative,-BK,-Metarules) is
%!	det.
%
%	Data about a Target theory from the current experiment file.
%
%	Target is the predicate indicator of the predicate to be
%	learned.
%
%	experiment_data/5 expects an experiment file to be loaded into
%	memory and will fail without warning otherwise.
%	initialise_experiment/0 should be called before it, and
%	cleanup_experiment/0 after it if cleanup is required between
%	experiments.
%
experiment_data(T,Pos,Neg,BK,MS):-
	configuration:experiment_file(P,M)
	,user:use_module(P)
	,findall(Ep
		,M:positive_example(T,Ep)
		,Pos_)
	,sort(Pos_, Pos)
	,findall(:-En
		,M:negative_example(T,En)
		,Neg_)
	,sort(Neg_, Neg)
	,once(M:background_knowledge(T,BK))
	,once(M:metarules(T,MS)).



%!	print_clauses(+Clauses) is det.
%
%	Print a list of Clauses to standard output.
%
print_clauses(L):-
	\+ is_list(L)
	,!
	,print_clauses([L]).
print_clauses(Cs):-
	forall(member(C,Cs)
	      ,(copy_term(C,C_)
	       ,numbervars(C_)
	       ,write_term(C_, [fullstop(true)
			       ,nl(true)
			       ,numbervars(true)
			       ,quoted(true)
			       ])
	       )
	      ).



%!	debug_clauses(+Topic,+Clauses) is det.
%
%	Debug a list of Clauses if Topic is being debugged.
%
debug_clauses(T,L):-
	\+ is_list(L)
	,!
	,debug_clauses(T,[L]).
debug_clauses(T,Cs):-
	forall(member(C,Cs)
	      ,(copy_term(C,C_)
	       ,numbervars(C_)
	       ,format(atom(A),'~W',[C_, [fullstop(true)
					 ,numbervars(true)
					 ,quoted(true)]
				    ])
	       ,debug(T,'~w',A)
	       )
	      ).



%!	program(+Symbols,+Module,-Program) is det.
%
%	Collect all clauses of a Program.
%
%	Symbols is the list of predicate indicators, F/A, of clauses in
%	Program.
%
%	Module is the definition module for Progam. This can be set to
%	user if the Program is not defined in a module.
%
%	Program is a list of all the clauses of the predicates in
%	Symbols.
%
%	@tbd This doesn't attempt to sort the list of Symbols to exclude
%	duplicates- if the same Symbol is passed in more than once, the
%	same definition will be included that many times in Programs.
%
program(F/A,M,Ps):-
	!
	,program([F/A],M,Ps).
program(Ss,M,Ps):-
	findall(P
	       ,(member(F/A,Ss)
		,functor(H,F,A)
		,M:clause(H,B)
		,(   B == true
		 ->  P = H
		 ;   P = (H:-B)
		 )
		)
	       ,Ps).



%!	closure(+Progam_Symbols,+Module,-Closure) is det.
%
%	Collect all clauses of a program and its Closure.
%
%	As program/3, but also collects the definitions of programs in
%	the closure of a progam.
%
%	Progam_Symbols is a list of predicate symbols and arities, F/A,
%	of clauses in a program. Closure is the set of definitions of
%	the Symbols in Program_Symbols, and the definitions of the
%	programs in the closure of each program in Program_Symbols.
%
%	Module is the definition module of each program in Closure, or
%	a module importing that module. To ensure each program in
%	Closure is accessible the best thing to do is to export
%	everything to the user module.
%
%	Example
%	=======
%	==
%	?- closure([ancestor/2],user,_Cs),forall(member(P,_Cs),print_clauses(P)).
%
%	ancestor(A,B):-parent(A,B).
%	ancestor(A,B):-parent(A,C),ancestor(C,B).
%	parent(A,B):-father(A,B).
%	parent(A,B):-mother(A,B).
%	father(stathis,kostas).
%	father(stefanos,dora).
%	father(kostas,stassa).
%	mother(alexandra,kostas).
%	mother(paraskevi,dora).
%	mother(dora,stassa).
%	true.
%	==
%
closure(Ss,M,Cs):-
	closure(Ss,[],_Ps,M,[],Cs_)
	,reverse(Cs_, Cs).

%!	closure(+Symbols,+Path_Acc,-Path,+Module,+Acc,-Closure) is det.
%
%	Business end of closure/3.
%
closure([],Ps,Ps,_M,Cs,Cs):-
	!.
closure([S|Ss],Ps_Acc,Ps_Bind,M,Acc,Bind):-
	\+ memberchk(S,Ps_Acc)
	,!
	,program(S,M,Cs)
	,closure(Ss,[S|Ps_Acc],Ps_Acc_,M,[Cs|Acc],Acc_)
	,program_symbols(Cs,Ss_)
	,closure(Ss_,Ps_Acc_,Ps_Bind,M,Acc_,Bind).
closure([_S|Ss],Ps,Ps_Acc,M,Acc,Bind):-
	closure(Ss,Ps,Ps_Acc,M,Acc,Bind).


%!	program_symbols(+Program,-Symbols) is det.
%
%	Collect symbols of body literals in a Program.
%
program_symbols(Ps,Ss):-
	clauses_literals(Ps,Ls)
	,setof(F/A
	      ,L^Ls^(member(L,Ls)
		    ,functor(L,F,A)
		    )
	      ,Ss).
