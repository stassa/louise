:-module(auxiliaries, [list_encapsulated_problem/1
		      ,list_mil_problem/1
		      ,initialise_experiment/0
		      ,assert_program/3
		      ,erase_program_clauses/1
		      ,experiment_data/5
		      ,print_clauses/1
		      ,program/3
		      ]).

:-user:use_module(lib(term_utilities/term_utilities)).
:-user:use_module(lib(program_reduction/program_reduction)).


%!	list_encapsulated_problem(+Target) is det.
%
%	Pretty-print the encapsulation of a MIL problem.
%
%	Target is the symbol and arity of the target predicate in the
%	encapsulated MIL problem to be listed.
%
list_encapsulated_problem(T):-
	experiment_data(T,Pos,Neg,BK,MS)
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
	,expanded_metarules(MS,MS_)
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



%!	assert_program(+Module,+Program,-Clause_References) is det.
%
%	As assert_program/2 but also binds a list of Clause_References.
%
assert_program(M,Ps,Rs):-
	assert_program(M,Ps,[],Rs).

assert_program(_,[],Rs,Rs):-
	!.
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
		,Pos)
	,findall(:-En
		,M:negative_example(T,En)
		,Neg)
	,M:background_knowledge(T,BK)
	,M:metarules(T,MS).



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
