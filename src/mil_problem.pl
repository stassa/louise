:-module(mil_problem, [metarule_parts/5
		      ,expanded_metarules/2
		      ,metarule_expansion/2
		      ,encapsulated_problem/5
		      ,encapsulated_bk/2
		      ,examples_target/2
		      ,encapsulated_clauses/2
		      ,applied_metarules/3
		      ,metarule_application/2
		      ,excapsulated_clauses/3
		      ]).

:-use_module(configuration).
:-use_module(lib(term_utilities/term_utilities)).
:-use_module(src(dynamic_learning)).
:-use_module(src(auxiliaries)).
:-use_module(src(metarules_parser)).

/** <module> Predicates to transform a MIL problem.

*/

%!	metarule_parts(?Metarule,?Id,?Sub,?Head,?Body) is nondet.
%
%	Extract the parts of a Metarule in the dynamic database.
%
%	Metarule is an encapsulated metarule in the dynamic database. It
%	is expanded, as a clause of m/n, where n one of the arities of
%	metarule and expanded metarules in the db.
%
%	Id is the ide of Metarule- a name, number, etc atomic id to help
%	find the metarule in the program database.
%
%	Sub is the substitution atom at the head of the encapsulated
%	metarule.
%
%	Head and Body are the encapsulated head and body literals of the
%	Metarule, respectively.
%
%	Note that one of Id, Metarule or Sub must be at least partly
%	instantiated or metarule_parts/5 will fail silently.
%
%	Use this predicate to access encapsulated and expanded metarules
%	in their internal representation, as clauses of m/n in the
%	dynamic database.
%
%	_DO NOT USE_ metarule_expansion/2 to access extended metarules
%	in the dynamic database - for one thing, metarule_expansion/2
%	cannot access _extended_ metarules (because they are
%	automatically created and named and have no corresponding
%	metarule/n declaration in the configuration). For another, it is
%	only there to transform metarules from their user-friendly
%	configuration representation to the internal m/n representation.
%
metarule_parts(Id,M,Sub,H,B):-
	ground(Id)
	,!
	,metarule_parts_(Id,M,Sub,H,B).
metarule_parts(Id,M,Sub,H,B):-
	nonvar(M)
	,!
	,M = (Sub:-_)
	,Sub =.. [m,Id|_Ps]
	,metarule_parts_(Id,M,Sub,H,B).
metarule_parts(Id,M,Sub,H,B):-
	ground(Sub)
	,Sub =.. [m,Id|_Ps]
	,metarule_parts_(Id,M,Sub,H,B).

%!	metarule_parts_(+Id,?Metarule,?Sub,-Head,-Body) is det.
%
%	Business end of metarule_parts/5.
%
%	metarule_parts/5 is responsible for grounding Id, depending on
%	its own bindings.
%
metarule_parts_(Id,M,Sub,H,B):-
	ground(Id)
	,user:current_predicate(m,Sub)
	,Sub =.. [m,Id|_Ps]
	,clause(Sub,(H,B))
	,M = (Sub:-(H,B)).



%!	expanded_metarules(+Ids,-Encapsulated) is det.
%
%	Encapsulate a set of metarules.
%
%	Metarules is a list of metarule definitions to be expanded by
%	metarule_expansion/2.
%
%	If Ids is a list of metarule names, only definitions of the
%	named metarules are bound to Metarules.
%
%	If Ids is a free variable, it is bound to a list of the names of
%	all Metarules known to the system.
%
expanded_metarules(Ids,Ms):-
	var(Ids)
	,!
	,findall(Id-M
	       ,metarule_expansion(Id,M)
	       ,Ids_Ms)
	,pairs_keys_values(Ids_Ms,Ids,Ms).
expanded_metarules(Ids,Ms):-
	is_list(Ids)
	,findall(M
	       ,(member(Id,Ids)
		,metarule_expansion(Id,M)
		)
	       ,Ms).



%!	metarule_expansion(?Id,-Metarule) is nondet.
%
%	Expand a Metarule with the given Id.
%
%	Performs a translation beween the user-level, user-friendly
%	atomic representation of metarules in metarule/2 clauses, and
%	Louise's internal representation as an expanded metarule with a
%	metasubstitution atom as a head literal and a set of
%	encapsualted body literals.
%
%	@tbd I've been meaning to change the name of this predicate, and
%	of expanded_metarules/1 to "metarule_translation/2" and
%	"translated_metarules/1" for a while now to avoid confusion with
%	metarule extension predicates' names.
%
%	@tbd Obviously this is just a renaming of parsed_metarule/2.
%	Another reason to rename it, or just get rid of it completely.
%
metarule_expansion(Id, M):-
	parsed_metarule(Id,M).


%!	encapsulated_problem(+Pos,+Neg,+BK,+MS,-Ps)
%!	is det.
%
%	Encapsualte a MIL problem.
%
%	Pos and Neg are lists of example atoms; Pos are positive
%	examples and Neg are negative examples, of the form :-E, where E
%	an atom.
%
%	BK is a list of predicate symbols and arities of BK predicates.
%
%	Metarules is a list of constants, the names of metarules in the
%	problem.
%
%	Ps is a list [Pos_, Neg_, BK_, MS_] where elements are the
%	encapsulations of the positive and negative examples, BK
%	definitions, and Metarules, respectively.
%
%	@tbd Encapsulated forms need documentation.
%
encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_]):-
	encapsulated_bk(BK,BK_)
	,expanded_metarules(MS,MS_)
	,encapsulated_clauses(Pos,Pos_)
	,encapsulated_clauses(Neg,Neg_).



%!	encapsulated_bk(+Background,-Encapsulated) is det.
%
%	Encapsulate a list of Background definitions.
%
encapsulated_bk(BK,Es):-
	closure(BK, user, Ps)
	,findall(Cs_
	       ,(member(Cs, Ps)
		,encapsulated_clauses(Cs,Cs_)
		)
	       ,Es_)
	,flatten(Es_, Fs)
	,hide_bk_closure(Fs,BK,Es).


%!	hide_bk_closure(+Closure,+Signature,-Hidden) is det.
%
%	Hide programs in the BK's Closure from the learning process.
%
%	Encapsulating the clauses of predicates in the BK and predicates
%	in the closure of predicates in the BKusing the same
%	encapsulation predicate makes it impossible to separate the
%	symbols in the BK, that should be used in the clauses of a
%	hypothesis, from the symbols in the BK's closure, that should
%	not.
%
%	The solution implemented in this predicate is to encapsulate the
%	clauses of predicates in the BK and the predicates in its
%	closure using different encapsulation predicates in a
%	post-processing step, i.e. after everything has already been
%	encapsulated using the BK encapsulation predicate.
%
%	Specifically, the BK encapsulation symbol is 'm' and the
%	BK-closure encapsulation symbol is 'p'.
%
%	The result of "hiding" the BK's closure in this way can be seen
%	with list_encapsulated_problem/1.
%
%	@tbd Now that there are two encapsulation predicates,
%	hard-coding them throughout the code, as is currently done, is
%	just begging for bugs. The two encapsulation predicates should
%	be formally defined as such, i.e. with a predicate
%	encapsulation_predicate(Purpose, Symbol) or some such, and
%	obtained from this predicate wherever needed.
%
%	@tbd Encapsulating non-BK predicates in a post-processing step
%	once they've all been encapsulated using the BK encapsulation
%	predicate already is ... not optimal. It will take a bit of work
%	maybe to add a couple of arguments to encapsulated_clause/2 etc
%	to select the encapsulation predicate during the initial
%	encapsulation step- but that's the best way to do it.
%
hide_bk_closure(Cs,PS,Cs_):-
	hide_bk_closure(Cs,PS,[],Cs_).

%!	hide_bk_closure(+Closure,+Signature,+Acc,-Hidden) is det.
%
%	Business end of hide_bk_closure/3.
%
%	@tbd This sorts the lists of clauses in the end - why? Is it to
%	remove duplicate clauses? That wouldn't work given that they're
%	probably not ground. Is it to order clauses in a standard
%	order? But that would change the semantics of a program. What's
%	up?
%
hide_bk_closure([],_PS,Acc,Cs):-
	sort(Acc,Cs)
	,!.
hide_bk_closure([C|Cs],PS,Acc,Bind):-
	hide_clause(C,PS,C_)
	,hide_bk_closure(Cs,PS,[C_|Acc],Bind).


%!	hide_clause(+Clause,+Signature,-Hidden) is det.
%
%	Hide non-BK literals in a Clause.
%
%	Clause is an encapsulated clause of a BK predicate.
%
%	The encapsulation predicate of literals in Clause whose symbol
%	is not in Signature is changed to the encapsulation predicate of
%	the BK closure, rather than the BK encapsulation predicate.
%
hide_clause(C,PS,C_):-
	clause_literals(C,Ls)
	,hide_literals(Ls,PS,[],Ls_)
	,once(list_tree(Ls_,T))
	,(   T = (H,B)
	 ->  C_ = (H:-B)
	 % T is an atom
	 ;   T \== (_,_)
	 ->  C_ = T
	 ).


%!	hide_literals(+Literals,+Signature,+Acc,-Hidden) is det.
%
%	Hide non-BK Literals
%
%	Literals is a list of encapsulated literals in a clause
%	processed by hide_clause/3.
%
%	Each literal in Literals is "hidden" from the learning process
%	by replacing its encapsulation predicate with the closure
%	encapsulation predicate.
%
hide_literals([],_PS,Acc,Ls):-
	reverse(Acc,Ls)
	,!.
hide_literals([L|Ls],PS,Acc,Bind):-
	L =.. [m,F|As]
	,length(As,A)
	,\+ memberchk(F/A,PS)
	,!
	,L_ =.. [p,F|As]
	,hide_literals(Ls,PS,[L_|Acc],Bind).
hide_literals([L|Ls],PS,Acc,Bind):-
	hide_literals(Ls,PS,[L|Acc],Bind).



%!	examples_target(+Examples,-Target) is det.
%
%	Extract the symbol and arity from Examples of a Target.
%
examples_target([E|_Es],F/A):-
	E =.. [m,F|As]
	,!
	,length(As,A).
examples_target([E|_Es],F/A):-
	functor(E,F,A).



%!	encapsulated_clauses(+Clauses, -Encapsulated) is det.
%
%	Encapsulate a list of Clauses.
%
encapsulated_clauses(Cs,Cs_):-
	encapsulated_clauses(Cs, [], Cs_).

%!	encapsulated_clauses(+Clauses,+Acc,-Encapsulated) is det.
%
%	Business end of encapsulated_clauses/2.
%
encapsulated_clauses([],Acc,Cs):-
	reverse(Acc, Cs)
	,!.
encapsulated_clauses([C|Cs], Acc, Bind):-
	encapsulated_clause(C,C_)
	,encapsulated_clauses(Cs,[C_|Acc],Bind).


%!	encapsulated_clause(+Clause, -Encapsulated) is det.
%
%	Encapsulate a Clause.
%
encapsulated_clause(C, C_):-
	encapsulated_clause(C, [], C_).

%!	encapsulated_clause(+Clause, +Acc, -Encapsulated) is det.
%
%	Business end of encapsulated_clause/2.
%
encapsulated_clause(:-((L,Ls)),Acc,C_):-
% Definite goal; L is the first literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,encapsulated_clause(:-(Ls),[:-L_|Acc],C_).
encapsulated_clause(:-(L),Acc,C):-
% Definite goal: L is the single remaining literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,reverse([:-L_|Acc],Ls)
	,once(list_tree(Ls,C)).
encapsulated_clause(H:-B,[],H:-B):-
% Definite clause; H is the head of a built-in predicate.
	built_in_or_library_predicate(H)
	,!.
encapsulated_clause((L,Ls),Acc,C_):-
% Definite clause; L is an atom of a built-in predicate.
	built_in_or_library_predicate(L)
	,!
	,encapsulated_clause(Ls,[L|Acc],C_).
encapsulated_clause(L,Acc,(H:-Bs)):-
% Definite clause; L is an atom of a built-in predicate.
	L \= (_,_)
	,built_in_or_library_predicate(L)
	,!
	,reverse([L|Acc], Ls)
	,once(list_tree(Ls,(H,Bs))).
encapsulated_clause(L:-Ls,Acc,C_):-
% Definite clause; L is the head literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,encapsulated_clause(Ls,[L_|Acc],C_).
encapsulated_clause((L,Ls),Acc,C_):-
% Definite clause; L is the next body literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,encapsulated_clause(Ls,[L_|Acc],C_).
encapsulated_clause(L,[],L_):-
% Unit clause: the accumulator is empty.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]].
encapsulated_clause(L,Acc,(H:-Bs)):-
% Definite clause; L is the last body literal.
	L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,reverse([L_|Acc], Ls)
	,once(list_tree(Ls,(H,Bs))).



%!	applied_metarules(+Metasubstitutions,+Metarules,-Applied) is
%!	det.
%
%	Apply a list of Metasubstitutions to corresponding Metarules.
%
%	The list of Metasubstitutions is normally the specialised Top
%	program.
%
%	@tbd To avoid having to write the Metarules to the dynamic
%	database just after we went to all this trouble to not do
%	that, this predicate could just operate on a list of key-value
%	pairs similar to the one returned by generalise/3.
%
applied_metarules(Ss,MS,Ms):-
	assert_program(user,MS,Rs)
	,findall(P
		,(member(S,Ss)
		 ,metarule_application(S,P)
		 )
		,Ms)
	,erase_program_clauses(Rs).


%!	metarule_application(+Metasubstitution,-Projection) is det.
%
%	Project a Metasubstitution onto a fitting metarule.
%
%	@tbd This is not strictly necessary and is left behind after
%	work to remove signature atoms from expanded metarules. The
%	right way to do this would now be to call metarule_parts/5, but
%	that will cause problems with extended metarules and
%	theorem_prover(tp) so it stays for now.
%
metarule_application_(S,H:-B):-
% TODO: is this needed, after switching to the metarule/2 format?
	S =.. [m,Id|Ps]
	,Mh =.. [metarule,Id|Ps]
	,clause(Mh,(H,B))
	,!.
metarule_application(S,H:-B):-
% Extended metarules don't have metarule/n heads!
% They're only in the database as m/n clauses.
	S =.. [m,_Id|_Ps]
	,clause(S,(H,B)).



%!	excapsulated_clauses(+Target, +Clauses, -Excapsulated) is det.
%
%	Remove encapsulation from a list of Clauses.
%
%	Only clauses of the Target predicate or invented predicates
%	based on the Target predicate, are excapsulated- clauses of
%	metarules and background definitions are dropped silently.
%
excapsulated_clauses(T, Cs, Cs_):-
	excapsulated_clauses(T,Cs,[],Cs_).

%!	excapsulated_clauses(+Target,+Acc,-Excapsulated) is det.
%
%	Business end of excapsulated_clauses/3.
%
excapsulated_clauses(_T,[],Acc,Es):-
	reverse(Acc, Es)
	,!.
excapsulated_clauses(T,[C|Cs],Acc,Bind):-
	excapsulated_clause(T,C,C_)
	,!
	,excapsulated_clauses(T,Cs,[C_|Acc],Bind).
excapsulated_clauses(T,[_C|Cs],Acc,Bind):-
	excapsulated_clauses(T,Cs,Acc,Bind).


%!	excapsulated_clause(+Target,+Clause,-Excapsulated) is det.
%
%	Excapsulate a single Clause of the Target predicate.
%
excapsulated_clause(T,C,C_):-
	excapsulated_clause(T,C,[],C_).

%!	excapsulated_clause(+Target,+Clause,+Acc,-Excapsulated) is det.
%
%	Business end of excapsulated_clause/3.
%
excapsulated_clause(T/A,H:-Bs,Acc,Bind):-
% Definite clause; H is the head literal.
	H =.. [m|[S|As]]
	,target_or_invention(T,S)
	,length(As,A)
	,!
	,H_ =.. [S|As]
	,excapsulated_clause(T,Bs,[H_|Acc],Bind).
excapsulated_clause(T,(L,Ls),Acc,Bind):-
% Definite clause: L is the next body literal.
	!
	,L =.. [m|[F|As]]
	,L_ =.. [F|As]
	,excapsulated_clause(T,Ls,[L_|Acc],Bind).
excapsulated_clause(T/A,L,[],L_):-
% Unit clause: the accumulator is empty.
	!
        ,L =.. [m|[S|As]]
	,target_or_invention(T,S)
	,length(As, A)
	,ground(S)
	,L_ =.. [S|As].
excapsulated_clause(_T,(L),Acc,(H:-Bs)):-
% Definite clause: L is the last literal.
	L =.. [m|[F|As]]
	,L_ =.. [F|As]
	,reverse([L_|Acc],Ls)
	,once(list_tree(Ls,(H,Bs))).


%!	target_or_invention(+Target,+Symbol) is det.
%
%	True when Symbol is Target or an invention from Target.
%
%	@tbd This expects that invented predicates' symbols will have
%	the same functor as the Target predicate but with a numeric
%	index appended to it by an underscore, '_'. This could be
%	enforced a little more strictly through the project.
%
target_or_invention(T,T):-
	!.
target_or_invention(_,S):-
	atom_chars(S,['$'|As])
	,number_chars(_N,As).
/* Earlier format but might use again
target_or_invention(T,S):-
	atomic_list_concat([T,A],'_',S)
	,atom_number(A,_N).
*/
/* Alternative- might use later
target_or_invention_(_,S):-
	atomic_list_concat(['$',A],'_',S)
	,atom_number(A,_N).
*/
