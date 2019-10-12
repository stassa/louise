:-module(mil_problem, [metarule_parts/5
		      ,expanded_metarules/2
		      ,metarule_expansion/2
		      ,extended_metarules/2
		      ,metarule_extension/3
		      ,encapsulated_problem/5
		      ,encapsulated_bk/2
		      ,examples_target/2
		      ,encapsulated_clauses/2
		      ,unfolded_metasubs/2
		      ,metarule_projection/2
		      ,excapsulated_clauses/3
		      ]).

:-use_module(configuration).
:-use_module(lib(term_utilities/term_utilities)).

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
%	instantiated or metarule_parts/6 will fail silently.
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
%	@tbd In line 5 of this predicate strip_module/3 is called to
%	allow metarules defined in experiment files to be used without
%	stumbling over complicated patterns of module qualifiers (which
%	will be different in their head and body literals).
%
%	@tbd After the removeal of signature atoms this predicate now
%	only changes the metarule/n symbol of metarules declared in the
%	configuration module and experiment files to m/n (and strips the
%	module qualifiers from it). In other words, it transforms
%	metarules from the user-friendly (-er) representation used in
%	configuration and experiment files to Louise's internal
%	representation that is more appropriate for learning.
%
metarule_expansion(Id,Mh_:-Mb_):-
	configuration:current_predicate(metarule,Mh)
	,Mh =.. [metarule,Id|Ps]
	,Mh_ =.. [m,Id|Ps]
	,clause(Mh,Mb)
	,strip_module(Mb,_M,Mb_).


%!	extended_metarules(+Metarules,-Extended) is det.
%
%	Extend a set of Metarules.
%
%	@tbd This is an expensive predicate. First, it collects lists of
%	metarules, two expanded metarules and their extension, as
%	key-value pairs, where the keys are the names of the metarules;
%	then, it flattens this list and sorts it to remove elements with
%	duplicate keys; finally, it splits the resulting sorted list to
%	keys and values lists and returns the values. The reason for all
%	this is the usual problem with sorting definite clauses with
%	variables: terms that are identical up to renaming of variables
%	appear different to sort/4 because of the "age" of variables
%	(when they were created). Rather than implementing a sorting
%	procedure that ignores variable age, it's much simpler to do
%	what is done here. The high cost is mitigated by the fact that
%	this predicate only needs to be called once at the start of
%	learning.
%
extended_metarules(MS,Es):-
	configuration:extend_metarules(E)
	,extended_metarules(MS,Es,E).

%!	extended_metarules(+Ids,-Extended,+N) is det.
%
%	Business end of extended_metarules/2.
%
extended_metarules(MS,Es,1):-
	!
	,findall([M1-M1_,M2-M2_,N-M]
	       ,(member(M1,MS)
		,member(M2,MS)
		,metarule_expansion(M1,M1_)
		,metarule_expansion(M2,M2_)
		,metarule_extension(M1_,M2_,M)
		,M = (H:-_B)
		,H =.. [m,N|_As]
		)
	       ,Es_)
	,flatten(Es_,Es_f)
	,sort(1,@<,Es_f,Es_s)
	%,Es_f = Es_s
	,pairs_keys_values(Es_s,_Ns,Es).
extended_metarules(MS,Es,N):-
	integer(N)
	,!
	,extended_metarules(MS,Es_1,1)
	,extended_metarules_(1,N,Es_1,Es_N)
	,append(Es_1,Es_N,Es_)
	% Sorting by first argument of first literal
	% To remove metarules with duplicate names.
	,sort([1,1],@<,Es_, Es).


%!	extended_metarules(+K,+N,+Metarules,-Extended) is det.
%
%	Extend a set of Metarules up to N times.
%
%	Used by extended_metarules/3 to extend metarules more than once.
%
%	K is the current extension counter, initialised to 1 on call.
%
extended_metarules_(N,N,Ms,Ms):-
	!.
extended_metarules_(I,N,Acc,Bind):-
	findall(M3
	       ,(member(M1,Acc)
		,member(M2,Acc)
		% Used for debugging.
		/*,M1 = (H_1:-_)
		,M2 = (H_2:-_)
		,H_1 =.. [m,Id_1|_]
		,H_2 =.. [m,Id_2|_]
		,format('Extending ~w by ~w~n', [Id_1,Id_2])
		*/
		,metarule_extension(M1,M2,M3)
		)
	       ,Ms_I)
	,succ(I,I_)
	,extended_metarules_(I_,N,Ms_I,Bind).



%!	metarule_extension(+Metarule1,+Metarule2,-Metarule3) is det.
%
%	Extend two metarules by unfolding them on each other.
%
%	Exending two metarules results in a new metarule with the head
%	literal of the first metarule and the body literals of both
%	minus the literal used to unfold one metarule on the other.
%
%	Unfolding two metarules involves unifying each body literal of
%	one metarule with the head literal of the second metarule and
%	then concatenating their body literals minus the body literal
%	unified with the head literal of the second metarule.
%
metarule_extension((M1:-B1),(M2:-B2),(M3:-B3)):-
	C = c(0)
	,unfold(B1,B2,B3)
	,metasubs_extension(C,M1,M2,B3,M3).


%!	metasubs_extension(+Count,+Metasub1,+Metasub2,+Body,-Metasub3)
%!	is det.
%
%	Create a named metasubstitution for a metarule extension.
%
%	Ensures that the metasubstitution atom, Metasub3, of a metarule
%	extension has a meaningful name and the correct set of
%	existentially quantified variables.
%
%	Metasub1 and Metasub2 are the metasubstitution atoms of two
%	metarules being extended. Body is the vector of body literals of
%	the extended metarules.
%
%	Count is a term c(I) used as a destructively updateable counter.
%	I counts the number of times metasubs_extension/5 is entered on
%	backtracking since the first binding of C, and, therefore, the
%	number of times the same two metasubstitutions have been
%	extended. If metasubs_extension/5 is called multiple times on
%	backtracking, the current value of I is appended to the end of
%	the name of the metarule extension created by extension_name/3.
%
%	@tbd As this entire call stack, this predicate too is insanely
%	inefficient, repeating the same calculation multiple times etc.
%	Perhaps use tabling? Or just find a better way to do all this.
%
metasubs_extension(C,M1,M2,B3,M3):-
	M1 =.. [m,N1|_Ps1]
	,M2 =.. [m,N2|_Ps2]
	,extension_name(N1,N2,N)
	,arg(1,C,I)
	,succ(I,I_)
	,(   I = 0
	 ->  N3 = N
	 ;   atomic_list_concat([N,I_],'_',N3)
	 )
	,nb_setarg(1,C,I_)
	,existential_vars(B3,Ps3)
	,M3 =.. [m,N3|Ps3].


%!	existential_vars(+Literals,-Variables) is det.
%
%	Collect Existentially quantified Variables in a set of Literals.
%
%	Literals is a "vector" of body literals of an encapsulated
%	metarule. Variables is a list of the their existentially
%	quantified variables.
%
%	@bug Actually, this only collects existentially quantified
%	_second order_ variables. Existentially quantified first order
%	variables will be lost in translation.
%
existential_vars(Ls,Vs):-
	existential_vars(Ls, [], Vs).

%!	existential_vars(+Litearls,+Acc,-Vars) is det.
%
%	Business end of existential_vars/2.
%
existential_vars((L),Vs,[S|Vs]):-
	L \= (_,_)
	,L =.. [m,S|_]
	,!.
	existential_vars((L,Ls), Acc, [S|Bind]):-
	L =.. [m,S|_]
	,existential_vars(Ls,Acc,Bind).


%!	extension_name(+Name1,+Name2,-Name) is det.
%
%	Compose the Name of a metarule extension.
%
extension_name(N1,N2,N3):-
	atomic_list_concat([N1,N2],'_',C_)
	,atomic_list_concat(Ns,'_',C_)
	,once(atoms_counts(Ns,Cs))
	,atomic_list_concat(Cs,'_',N3).


%!	atoms_counts(+Atoms,-Counts) is det.
%
%	Count the atomic elements in a metarule name.
%
%	Implements run-length encoding of a list of Atoms, but single
%	atoms are not associated to a count and the list is not an
%	association list (as in 99 Prolog problems, where "run-length
%	encoding" comes from). The purpose is to count the number of
%	consecutive identical atoms (possibly separated by "_") in the
%	name of a metarule extension, in order to name it something more
%	concise than "chain_chain_chain_ ... _chain_chain_chain...
%	_unit" etc.
%
atoms_counts([A|As],Cs):-
	atoms_counts(As,[A],Cs).

%!	atoms_counts(+Atoms,+Acc,-Counts) is det.
%
%	Business end of atoms_counts/2.
%
atoms_counts([],[A-N],[A,N]).
% List of single atom
atoms_counts([],[A],[A]).
% Single atom.

atoms_counts([],[A-N|Acc],Cs):-
% No more atoms
	reverse([N,A|Acc],Cs).
atoms_counts([],Acc,Cs):-
	reverse(Acc,Cs).
atoms_counts([A],[A-N|Acc],Cs):-
	succ(N,N_)
	,reverse([N_,A|Acc],Cs).

atoms_counts([A,An|As],[A|Acc],Bind):-
% If an atom is followed by a number
% it's an expanded metarule already named by count
	atom_number(An, N)
	,atoms_counts(As,[A,N,A|Acc],Bind).

atoms_counts([A|As],[A],Bind):-
% New atom - already seen
	atoms_counts(As,[A-2],Bind).
atoms_counts([A|As],[A|Acc],Bind):-
	atoms_counts(As,[A-2|Acc],Bind).

atoms_counts([A|As],[A-N|Acc],Bind):-
% New atom - continued count
	succ(N,N_)
	,atoms_counts(As,[A-N_|Acc],Bind).
atoms_counts([B|As],[A-N|Acc],Bind):-
	atoms_counts(As,[B,N,A|Acc],Bind).

atoms_counts([B|As],[A|Acc],Bind):-
% New atom - first time seen
	atoms_counts(As,[B,A|Acc],Bind).


%!	unfold(+Metarule1,+Metarule2,-Unfolded) is nondet.
%
%	Unfold two metarules on one another.
%
%	Unfodling here means that we resolve the two metarules so that
%	each of the body literals of Metarule1 is the literal resolved
%	upon.
%
%	Operatively, we take each body literal Li in Metarule1, unify
%	it with the head literal of Metarule2 and replace Li with the
%	body literals of Metarule2.
%
%	This nondeterminism in this predicate comes from the consecutive
%	resolution with each possible body literal in Metarule1.
%
%	@tbd This is a very naive and so inefficient way to do this.
%	There is a better way but it takes more work and I want to test
%	that this all works as expected first.
%
unfold(M1,M2,M):-
	once(list_tree([H1|B1],M1))
	,once(list_tree([H2|B2],M2))
	,select(H2,B1,B2,B3)
	,flatten([H1|B3], M_f),
	once(list_tree(M_f,M)).



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
	configuration:extend_metarules(E)
	,encapsulated_bk(BK,BK_)
	,(   E \== false
	 ->  extended_metarules(MS, MS_)
	 ;   expanded_metarules(MS,MS_)
	 )
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
	predicate_property(H,built_in)
	,!.
encapsulated_clause((L,Ls),Acc,C_):-
% Definite clause; L is an atom of a built-in predicate.
	predicate_property(L,built_in)
	,!
	,encapsulated_clause(Ls,[L|Acc],C_).
encapsulated_clause(L,Acc,(H:-Bs)):-
% Definite clause; L is an atom of a built-in predicate.
	L \= (_,_)
	,predicate_property(L,built_in)
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



%!	unfolded_metasubs(+Metasubstitutions,-Unfolded) is det.
%
%	Project a list of Metasubstitutions onto fitting metarules.
%
%	The list of Metasubstitutions is normally the specialised Top
%	program.
%
unfolded_metasubs(Ss,Ms):-
	findall(P
		,(member(S,Ss)
		 ,metarule_projection(S,P)
		 )
		,Ms).



%!	metarule_projection(+Metasubstitution,-Projection) is det.
%
%	Project a Metasubstitution onto a fitting metarule.
%
%	@tbd This is not strictly necessary and is left behind after
%	work to remove signature atoms from expanded metarules. The
%	right way to do this would now be to call metarule_parts/5, but
%	that will cause problems with extended metarules and
%	theorem_prover(tp) so it stays for now.
%
metarule_projection(S,H:-B):-
	S =.. [m,Id|Ps]
	,Mh =.. [metarule,Id|Ps]
	,clause(Mh,(H,B))
	,!.
metarule_projection(S,H:-B):-
% Extended metarules don't have metarule/n heads!
% They're only in the database as m/n clauses.
	S =.. [m,_Id|_Ps]
	,clause(S,(H,B)).



%!	excapsulated_clauses(+Target, +Clauses, -Excapsulated) is det.
%
%	Remove encapsulation from a list of Clauses.
%
%	Only clauses of the Target predicate are processed- clauses of
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
	H =.. [m|[T|As]]
	,length(As,A)
	,!
	,H_ =.. [T|As]
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
        ,L =.. [m|[T|As]]
	,length(As, A)
	,ground(T)
	,L_ =.. [T|As].
excapsulated_clause(_T,(L),Acc,(H:-Bs)):-
% Definite clause: L is the last literal.
	L =.. [m|[F|As]]
	,L_ =.. [F|As]
	,reverse([L_|Acc],Ls)
	,once(list_tree(Ls,(H,Bs))).

