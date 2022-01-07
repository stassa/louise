:-module(mil_problem, [metarule_parts/5
		      ,expanded_metarules/2
		      ,metarule_expansion/2
		      ,encapsulated/1
		      ,encapsulated_problem/5
		      ,encapsulated_bk/3
		      ,examples_targets/2
		      ,encapsulated_clauses/2
		      ,applied_metarules/3
		      ,excapsulated_clauses/3
		      ,target_or_invention/2
		      ]).

:-use_module(project_root(configuration)).
:-use_module(lib(term_utilities/term_utilities)).
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
%	@tbd Unusable, since encapsulated metarules are no longer
%	written to the dynamic database.
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



%!	expanded_metarules(?Ids,-Encapsulated) is det.
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
%	@tbd The mode (?,-) seems a little dangerous and in any case
%	there's the "all" option that does the same thing more or less.
%	Is it really necessary to allow Ids to be unbound on entry?
%
expanded_metarules(MS,MS):-
% Already expanded and encapsulated.
	\+ var(MS)
	,encapsulated(MS)
	,!.
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
	       ,Ms_)
	% Higher order metarule numbers end up [[caged]]
	,flatten(Ms_,Ms).



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
metarule_expansion(higher_order(Min,Max), M):-
% Higher order metarules are transformed to a list of integers.
	!
	,series(Min,Max,M).
metarule_expansion(N, N):-
% Something is asking for already-transformed higher order metarules to
% be expanded again- ignore it, they're just a list of integers.
	integer(N)
	,!.
metarule_expansion(Id, M):-
	parsed_metarule(Id,M).



%!	encapsulated(+Terms) is det.
%
%	True when Terms is a list of encapsulated Prolog terms.
%
%	Used by auxiliaries of encapsulated_problem/5 to allow the
%	elements of an already encapsulated MIL problem being passed to
%	learning predicates.
%
%	@tbd Similar analysis of a (Prolog) term to see if it's
%	encapsulaed is done ad-hoc in many places in the code. Perhaps
%	find them and replace them? For instance, see symbol/2 which
%	extracts the predicate symbol of a possible encapsulated term.
%	This could definitely be combined with this predicate, for
%	insance this predicate could return the encapsulated predicate
%	symbol.
%
encapsulated(Ts):-
	Ts = [H:-_B|_]
	,functor(H,m,_)
	,!.
encapsulated(Ts):-
% A negated term - probably a negative example.
	Ts = [:-T|_]
	,functor(T,m,_)
	,!.
encapsulated(Ts):-
	Ts = [T|_]
	,functor(T,m,_).



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
	examples_targets(Pos, Ss)
	,encapsulated_bk(BK,Ss,BK_)
	,expanded_metarules(MS,MS_)
	,encapsulated_clauses(Pos,Pos_)
	,encapsulated_clauses(Neg,Neg_).


%!	encapsulated_bk(+Background,+Symbols,-Encapsulated) is det.
%
%	Encapsulate a list of Background definitions.
%
%	Symbols is the set of predicate symbols of target predicates
%	(including invented predicates), used to ensure clauses of
%	target predicates in the closure of BK predicates are
%	encapsulated as target predicates, with functor m, and not as BK
%	auxiliaries with functor p.
%
encapsulated_bk([],_Ss,[]):-
	!.
encapsulated_bk(BK,_Ss,BK):-
% Already encapsulated.
	encapsulated(BK)
	,!.
encapsulated_bk(BK,Ts,Es):-
	(   closure(BK, user, Ps)
	 ->  true
	 ;   closure(BK, experiment_file, Ps)
	 ->  true
	 ;   throw('Missing BK definition of predicate in':BK)
	 )
	,findall(Cs_
	       ,(member(Cs, Ps)
		,encapsulated_clauses(Cs,Cs_)
		)
	       ,Es_)
	,flatten(Es_, Fs)
	,predicate_signature(Ts,Ss)
	,hide_bk_closure(Fs,Ss,Es).



%!	hide_bk_closure(+Closure,+Signature,-Hidden) is det.
%
%	Hide programs in the BK's Closure from the learning process.
%
%	Encapsulating the clauses of predicates in the BK and predicates
%	in the closure of predicates in the BK using the same
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
hide_bk_closure([],_PS,Acc,Cs):-
	!
       ,reverse(Acc,Cs).
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



%!	examples_targets(+Examples,-Targets) is det.
%
%	Extract symbols and arities from Examples of Targets.
%
%	Examples is a list of positive or negative examples,
%	encapsulated or not. Targets is a list of the predicate symbols
%	and arities of the target predicates in that list of examples.
%
examples_targets(Es,Ss):-
        setof(S
             ,E^Es^(member(E,Es)
                   ,symbol(E,S)
              )
             ,Ss).


%!	symbol(+Example,-Symbol) is det.
%
%	Derive the predicate symbol of an Example.
%
%	Example is a positive or negative example. It might be
%	encapsulated, or not. Symbol is a compound Symbol/Arity, the
%	symbol and arity of the Example's predicate (not m/n if
%	encapsulated).
%
symbol(H:-_B,F/A):-
	!
	,symbol(H,F/A).
symbol(:-E,F/A):-
% Encapsulated negative example.
        E =.. [m,F|As]
        ,!
        ,length(As,A).
symbol(:-H,F/A):-
% Not encapsulated negative example.
        functor(H,F,A)
        ,!.
symbol(E,F/A):-
% Encapsulated example.
        E =.. [m,F|As]
        ,!
        ,length(As,A).
symbol(H,F/A):-
% Not encapsulated example.
        functor(H,F,A).



%!	encapsulated_clauses(+Clauses, -Encapsulated) is det.
%
%	Encapsulate a list of Clauses.
%
encapsulated_clauses(Cs,Cs):-
% Already encapsulated.
	encapsulated(Cs)
	,!.
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
encapsulated_clause(:-(H:-Bs),Acc,:-C_):-
% Negated definite clause; a negative example.
	!
	,encapsulated_clause(H:-Bs,Acc,C_).
encapsulated_clause(:-((L,Ls)),Acc,:-C_):-
% Definite goal; L is the first literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,encapsulated_clause(Ls,[L_|Acc],C_).
encapsulated_clause(:-(L),[],:-L_):-
% Definite goal: L is the single literal.
	!
	,L =.. [F|As]
	,L_ =.. [m|[F|As]].
encapsulated_clause(H:-B,[],H:-B):-
% Definite clause; H is the head of a built-in predicate.
	built_in_or_library_predicate(H)
	,!.
encapsulated_clause((L,Ls),Acc,C_):-
% Definite clause; L is a body literal that is an atom of a built-in
% predicate.
	built_in_or_library_predicate(L)
	,!
	,encapsulated_clause(Ls,[L|Acc],C_).
encapsulated_clause(L,Acc,H:-Bs):-
% Definite clause; L is a literal that is an atom of a built-in
% predicate.
%TODO: why H:-Bs? We might have a definite goal in Acc.
	L \= (_,_)
	,built_in_or_library_predicate(L)
	,reverse([L|Acc], Ls)
	,once(list_tree(Ls,(H,Bs)))
	,!.
encapsulated_clause(L,[],L):-
% Definite clause; L is an atom of a built-in predicate.
% The accumulator is empty
	L \= (_,_)
	,built_in_or_library_predicate(L)
	,!.
encapsulated_clause(H:-Bs,Acc,H_:-Bs_):-
% Definite clause; H is the head literal.
	!
	,H =.. [F|As]
	,H_ =.. [m|[F|As]]
	,encapsulated_clause(Bs,Acc,Bs_).
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
encapsulated_clause(L,Acc,Ls_):-
% Definite clause; L is the last body literal.
	L =.. [F|As]
	,L_ =.. [m|[F|As]]
	,reverse([L_|Acc], Ls)
	,once(list_tree(Ls,Ls_)).



%!	applied_metarules(+Metasubstitutions,+Metarules,-Applied) is
%!	det.
%
%	Apply a list of Metasubstitutions to corresponding Metarules.
%
%	The list of Metasubstitutions is normally the specialised Top
%	program.
%
applied_metarules(Ss,_MS,Ms):-
	findall(Msub
		,(member(Sub,Ss)
		 ,applied_metasubstitution(Sub,Msub)
		 )
		,Ms).


%!	applied_metasubstitution(+Metasubstitution,-Applied) is det.
%
%	Apply a Metasubstitution atom to its corresponding metarule.
%
%	Abstracts the application of a Metasubstitution atom to its
%	corresponding metarule which may or may not have a body.
%
applied_metasubstitution(Sub-(Sub:-(H,B)), H:-B):-
	!.
applied_metasubstitution(Sub-(Sub:-(L)), L).



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


%!	excapsulated_clause(+Targets,+Clause,-Excapsulated) is det.
%
%	Excapsulate a single Clause of all learning Targets.
%
excapsulated_clause(T,C,C_):-
	excapsulated_clause(T,C,[],C_).

%!	excapsulated_clause(+Targets,+Clause,+Acc,-Excapsulated) is det.
%
%	Business end of excapsulated_clause/3.
%
excapsulated_clause(Ts,H:-Bs,Acc,Bind):-
% Definite clause; H is the head literal.
	H =.. [m|[S|As]]
	,length(As,A)
	,target_or_invention(Ts,S/A)
	,!
	,H_ =.. [S|As]
	,excapsulated_clause(Ts,Bs,[H_|Acc],Bind).
excapsulated_clause(Ts,(L,Ls),Acc,Bind):-
% Definite clause: L is the next body literal.
	!
	,L =.. [m|[F|As]]
	,L_ =.. [F|As]
	,excapsulated_clause(Ts,Ls,[L_|Acc],Bind).
excapsulated_clause(Ts,L,[],L_):-
% Unit clause: the accumulator is empty.
	!
        ,L =.. [m|[S|As]]
	,length(As, A)
	,target_or_invention(Ts,S/A)
	,ground(S)
	,L_ =.. [S|As].
excapsulated_clause(_Ts,(L),Acc,(H:-Bs)):-
% Definite clause: L is the last literal.
% We don't need to check the symbol again.
	L =.. [m|[F|As]]
	,L_ =.. [F|As]
	,reverse([L_|Acc],Ls)
	,once(list_tree(Ls,(H,Bs))).


%!	target_or_invention(+Targets,+Symbols) is det.
%
%	True when Symbols are Targets or inventions from Targets.
%
%	@tbd This expects that invented predicates' symbols will have
%	the same functor as the Target predicate but with a numeric
%	index appended to it by an underscore, '_'. This could be
%	enforced a little more strictly through the project.
%
%	TODO: pluralise.
%
target_or_invention(Ts,F/A):-
        memberchk(F/A,Ts)
	,!.
target_or_invention(_,S/_):-
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
