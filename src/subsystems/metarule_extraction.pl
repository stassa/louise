:-module(metarule_extraction, [bk_metarules/2
			      ,symbols_metarules/3
			      ,program_metarules/2
			      ]).

:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).
:-use_module(lib(lifting/lifting)).


/** <module> Extract metarules from first-order background knowledge.

Predicates in this module extract metarules from a definite program by
variabilising the program's clauses. Extracted metarules are
encapsulated and expanded into Louise's metarule notation, i.e.
into clauses where the head literal is a metasubstitution atom and the
first body literal is the encapsulated head literal of a metarule.

During metarule extraction predicate symbols in a program's clauses are
replaced with existentially quantified, second-order variables and
constants are replaced with universally quantified first order
variables. Identical terms are replaced with the same variable and any
variables pre-existing in a clauses are retained unchanged.

Note that, since all this is done in Prolog that has no concept of
existentially quantified variables or second-order terms, in practice
all variables in the output of predicates in this module are first-order
and (implicitly) universally quantified. That makes no difference in how
metarules are actually used.

A notable use of predicates in this module is to extract metarules from
the definitions of predicates declared as background knowledge for a
learning target.

Examples of Use
---------------

Predicate program_metarules/2 is used to extract metarules from an
arbitrary list of definite clauses:

==
?- _Ps = [ p(_X,_Y):- q(_X,_Z), r(_Z,_Y), p(a,b) ]
  , program_metarules(_Ps, _MS), print_clauses(_MS).

m(metarule_1,A,B):-m(A,C,D),m(B,C,E).
true.
==

Predicate symbols_metarules/3 is used to extract metarules from a list
of predicate symbols and arities. The second argument is the name of a
module where the given predicates are defined, or a module importing
those predicates' definition module:

==
?- symbols_metarules([symbols_metarules/3], user, _MS)
  ,print_clauses(_MS).

m(metarule_1,A,B,C):-m(A,D,E,F),m(B,D,E,G),m(C,G,F).
true.

% Definition of symbols_metarules/3 in this file:
% symbols_metarules(Ss,M,MS):-
%	program(Ss, M, Ps)
%	,program_metarules(Ps, MS).
==

Predicate bk_metarules/2 is used to extract metarules from the
definitions of predicates declared as background for a learning target.
Below, metarules are extracted from the definitions of background
predicates for grandfather/2, one of the learning targets in
data/examples/tiny_kinship.pl (its BK predicates are also defined in the
same module):

==
?- bk_metarules(grandfather/2, _MS), print_clauses(_MS).
m(metarule_1,A,B):-m(A,C,D),m(B,C,D).
m(metarule_2,A,B,C):-m(A,D,E),m(B,D,F),m(C,E,F).
m(metarule_3,A,B,C):-m(A,D,E),m(B,D,F),m(C,F,E).
true.

% BK declaration for grandfather/2 in examples/tiny_kinship.pl:
% background_knowledge(grandfather/2,[father/2,parent/2,husband/2,grandmother/2]).

% Definitions of BK predicates for grandfather/2 in tiny_kinship.pl):
% parent(X, Y):- father(X,Y).
% parent(X, Y):- mother(X,Y).
%
% husband(X,Y):- father(X,Z) ,mother(Y,Z).
% grandmother(A,B):- mother(A,C) ,parent(C,B).
==

In the examples above, note that metarules are given generic names
consisting of the constant "metarule", followed by an underscore and a
numeric index, indicating the order of the metarule in the list of
metarules output by bk_metarules/2. There is no guarantee that such
metarule IDs will be in any way unique: the user is required to give
them unique names, perhaps evern meaningful names.

Limitations
-----------

A current limitation of metarule extraction is that constants are not
replaced with existentially quantified first-order variables.

For example, the program p(a,B):- q(a,B) should result in a metarule
m(metarule_1,P,Q,A):- m(P,A,B), m(Q,A,B), replacing the constant 'a'
with the existentially quantified first-order variable A.

Instead, the current version of metarule extraction will construct a
metarule such as m(metarule_1,P,Q):- m(P,A,B), m(Q,A,B) where the
constant 'a' is replaced by a first-order universally quantified
variable, thereby subtly altering the structure of the original clause.
This limitation will be addressed in future versions of this module.

*/


%!	bk_metarules(+Target,-Metarules) is det.
%
%	Extract Metarules from background knowledge declared for Target.
%
%	Target is a learning target defined in the current experiment
%	file. Metarule is a list of metarule extracted from the
%	definitions of the predicates declared as background knowledge
%	for Target in the current experiment file.
%
%	Metarules are extracted by a call to symbols_metarules/3,
%	passing the predicate symbols of the background predicates, and
%	the module 'experiment_file'. Accordingly, only predicates
%	defined in the current experiment file will be used to extract
%	metarules.
%
bk_metarules(T, MS):-
	experiment_data(T,_Pos,_Neg,BK,_MS)
	,symbols_metarules(BK,experiment_file,MS).


%!	symbols_metarules(+Symbols,+Module,-Metarules) is det.
%
%	Extract a list of Metarules given a list of predicate Symbols.
%
%	Symbols is a list of predicate indicators, the symbols and
%	arities of predicates in a program from which mearules are to be
%	extracted. The program must be visible to the given Module (i.e.
%	either defined in that Module, or imported to it from another
%	module).
%
%	Metarules is a list of metarules extracted from the program
%	indicated by the predicate indicators in Symbols, by a call to
%	program_metarules/2.
%
symbols_metarules(Ss,M,MS):-
	program(Ss, M, Ps)
	,program_metarules(Ps, MS).


%!	program_metarules(+Program, -Metarules) is det.
%
%	Extract a list of Metarules from a first order Program.
%
%	Program is a list of first-order definite clauses. Metarules is
%	a list of metarules such that each non-unit clause in Program is
%	an instantiation of a Metarule in Metarules.
%
%	Metarules in the output list are encapsulated and expanded
%	into Louise's metarule notation. Metasubstitution atoms in
%	extracted metarules are given generic metarule IDs, consisting
%	of the constant "metarule" followed by an underscore and a
%	number, the position of the metarule in the list, Metarules.
%	These numbered metarule IDs are by no means guaranteed to be
%	unique and it is expected that the user will inspect the
%	metarules resulting from a call to this predicate and assign
%	unique names to the ones she wants to keep, perhaps even
%	meaningful names if that is at all possible.
%
%	Note that Metarules are not extracted from unit clauses. Such
%	metarules are usually of the form m(P,A,B) and not particularly
%	useful in practice.
%
%	@bug This predicate currently replaces constants in the head and
%	body literals of clauses with universally quantified variables,
%	when they should be replaced with existentially quantified
%	first-order variables added to the arguments of the
%	metasubstitution atom of the encapsulated metarule.
%
program_metarules(Ps, MS):-
	(   encapsulated(Ps)
	->  Es = Ps
	;   encapsulated_clauses(Ps, Es)
	)
	,lifted_program(Es,Ls)
	,setof(A:-(H,B)
	      ,Ls^H^Exs^(member(H:-B,Ls)
			,numbervars(H:-B)
			,existential((H,B),Exs)
			,metasubs_atom(Exs,A)
			)
	      ,CS_)
	,varnumbers(CS_,MS_)
	,numbered_metarules(MS_,MS).


%!	existential(+Literals, -Variables) is det.
%
%	Extract the existentially quantified Variables of Literals.
%
%	Literals is a set of literals of a metarule. Variables are the
%	second-order existentially quantified variables in each of its
%	literals.
%
existential(Ls,Es):-
	existential(Ls, [], Es).

%!	existential(+Literal,+Acc,-Variables) is det.
%
%	Business end of existential/2.
%
existential(L,Acc,Es):-
	L \= (_,_)
	,!
	,L =.. [m,P|_Vs]
	,reverse([P|Acc],Es).
existential((L,Ls),Acc,Bind):-
	L =.. [m,P|_Vs]
	,existential(Ls,[P|Acc],Bind).


%!	metasubs_atom(+Existential, -Atom) is det.
%
%	Constructe a metasubstitution Atom for a metarule.
%
%	Existential is the list of second-order existentially quantified
%	variables in metarule. Atom is a metasubstitution atom of the
%	form: m(,metarule,E1,...,En) where each Ei is a variable in
%	Existential.
%
%	@tbd Metasubstitution atoms are assigned the generic metarule
%	Id, "metarule". This is later indexed by a number relative to
%	the number of metarules extracted from a program by a call to
%	numbered_metarules/2.
%
metasubs_atom(Es,A):-
	A =.. [m,metarule|Es].


%!	numbered_metarules(+Metarules,-Numbered) is det.
%
%	Give a list of Metarules Numbered identifiers.
%
%	Metarules is a list of expanded metarules, with metasubstitution
%	atoms each having the generic Id "metarule". Numbered is the
%	same list of metarules where the generic "metarule" Id is
%	indexed with a number, the order of the mearule in the list
%	Metarules.
%
%	@tbd This predicate is called at the end of processing in
%	program_metarules/2 when all metarules that can be extracted
%	from an input program have been extracted and then sorted to
%	remove duplicates. As a result, numeric indices appended to
%	mearule Ids in Numbered begin at 1 and are unique with respect
%	to the set of metarules in the input program. However, there is
%	no guarantee that each indexed metarule Id will be unique across
%	a project, leta lone multiple extraction runs.
%
numbered_metarules(MS,MS_):-
	C = c(1)
	,findall(A_:-M
	       ,(member(A:-M,MS)
		,A =.. [m,ID|As]
		,arg(1,C,I)
		,atomic_list_concat([ID,I],'_',ID_I)
		,A_ =.. [m,ID_I|As]
		,succ(I,I_)
		,nb_setarg(1,C,I_)
		)
	       ,MS_).
