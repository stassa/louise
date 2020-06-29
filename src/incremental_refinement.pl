:-module(incremental_refinement, [learn_incremental/4
                                 ,learn_incremental/5
                                 ,incremental_refinement/5
                                 ,incremental_refinement/6
                                 ]).

:-use_module(src(louise)).
:-use_module(src(dynamic_learning)).

/** <module> Incremental refinement of background knowledge.

Predicates in this module implement incremental background knowledge
refinement, by which the predicate definitions in the background are
replaced by a hypothesis learned from that background knowledge and the
examples and metarules in an input MIL problem. This process is repeated
in successive steps in each of which a new background theory is formed
and replaces the previous one. The process continues until it is not
possible to derive any new hypotheses or a certain iteration is reached.
In the end of the process, the set of definitions invented in this way
is returned.

See incremental_refinement/6 for a more complete description of the
procedure. See learn_incremental/[3,5] for examples of use.

Incremental refinement is meant as a complement to the predicate
invention method implemented in dynamic_learning.pl. It can also be
useful as a dataset exploration and knowledge discovery tool.

*/


%!      learn_incremental(+Problem_1,+Problem_2,-Invented,-Program) is
%!      det.
%
%       Learn a Program by incremental refinement of a theory.
%
%	Problem_1 and Problem_2 are lists [P1,N1,B1,M1,K1] and
%	[P2,N2,B2,M2,K2], respectively, each representing a MIL problem,
%	where each Pi is a set of positive examples, each Ni is a set of
%	negative examples, each Bi is a set of predicate symbols and
%	arities of background predicates, each Mi is a set of atomic
%	metarule identifiers and each Ki an integer, the maximum number
%	of predicates to invent at each of the two stages of learning.
%
%	Invented and Program are sets of clauses learned in two stages
%	from the elements of Problem_1 and Problem_2.
%
%	In the first stage the background knowlede, B1, given in
%	Problem_1 is incrementally refined by a call to
%	incremental_refinement/5. The maximum number of predicates
%	invented in this stage is determined by K1. Clauses of any
%	predicates learned in this stage are bound to Invented.
%
%	In the secont stage, the clauses of the refined background
%	knowledge in Invented are combined with the background
%	knowledge, B2, in Problem_2 and a new program is learned by
%	dynamic learning. The maximun number of predicates invented in
%	this stage is determined by K2. Clauses of any predicates
%	learned in this stage are bound to Program.
%
%	The predicates in Invented and the background knowledge, B2, in
%	Problem_2 is combined as determined by the value of B2, as
%	follows.
%
%	If B2 is the atom "append" the contents of Invented are added to
%	the background knowledge, B1, given in Problem_1.
%
%	If B2 is the atom "invented", the contents of Invented are used
%	as background knowledge for the second stage.
%
%	If B2 is the term combine(Ls), where Ls is a list of predicates'
%	symbols and arities, the definitions of all predicates in Ls are
%	added to the contents of Invented and the resulting set is used
%	as background knowledge for the second stage.
%
%	Using the examples as background knowledge
%	------------------------------------------
%
%	In order for Louise to use the predicates of any of the
%	positive examples, Pos_1, given in Problem_1, to invent new
%	predicates in the first stage of incremental refinement, the
%	symbols of those predicates must be added to the background
%	knowledge, B1, given in Problem_1.
%
%	For example, in the following background declaration, the symbol
%	of the learning target path/2 is added to the background
%	predicate edge/2:
%
%	==
%	background_knowledge(path/2,[path/2,edge/2]).
%	==
%
%	This way, Louise can use the examples of path/2 as a partial
%	definition of the target and any predicates invented in the
%	first, refinement stage can be defined in terms of path/2.
%
%	The reason for this slight abuse of the experiment file format
%	is to simplify the implementation of learn_incremental/4 and
%	avoid serious abuse of the dynamic database in its
%	implementation.
%
%       Examples of calls
%       =================
%
%       Example experiment file
%       -----------------------
%
%	The examples below use the elements of the MIL problem defined
%	in data/examples/incremental_refinemnt.pl. The MIL problem
%	includes a single positive example of a target predicate path/2
%	and background knowledge consisting of atoms of edge/2. The
%	background knowledge represents the edges of a graph and path/2
%	reprsents a path between two nodes on that graph.
%
%	According to the note in the previous section, the single
%	positive example atom of path/2 is also added to the background
%	knowledge.
%
%	Note that while Louise can learn a hypothesis of path/2 with
%	dynamic learning without incremental refinement, the definition
%	learned requires 10 invented predicates. The definition learned
%	with incremental refinement is much more compact, requiring a
%	single invented predicate.
%
%	Example 1
%	---------
%
%	This example demonstrates the use of the "invented" atom in BK2.
%
%	The following call performs incremental refinement of the
%	background knowledge definitions of edge/2 and path/2 with an
%	invented predicate limit of 1 (i.e. a single predicates is
%	invented by incremental refinement). The definition of the
%	single invented predicate, '$1'/2, learned in this way is used
%	as the background knowledge for the second, dynamic learning,
%	attempt. No new predicates are invented in this second learning
%	attempt because the maximum number of invented predicates, J, is
%	set to 0. The result is a hypothesis of the target predicate,
%	path/2, defined in terms of '$1'/2.
%
%	==
%	?- _K = 1, _J = 0
%	,_BK2 = invented
%	,experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%	,learn_incremental([_Pos,_Neg,_BK,_MS,_K],[_Pos,_Neg,_BK2,_MS,_J],_Is,_Ps)
%	,print_clauses('Invented:',_Is), nl, print_clauses('Learned:',_Ps).
%	Invented:
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%
%	Learned:
%	path(A,B):-'$1'(A,B).
%	true.
%	==
%
%	Example 2
%	---------
%
%	This example demonsrates the use of the "append" atom in BK2.
%
%	The following call is as in Example 1, except this time the
%	definition of the single invented predicate '$1'/2 learned by
%	incremental refinement is added to the background knowledge
%	definitions of path/2 and edge/2 given in Problem_1.
%
%	==
%	?- _K = 1, _J = 0
%	,_BK2 = append
%	,experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%	,learn_incremental([_Pos,_Neg,_BK,_MS,_K],[_Pos,_Neg,_BK2,_MS,_J],_Is,_Ps)
%	,print_clauses('Invented:',_Is), nl, print_clauses('Learned:',_Ps).
%	Invented:
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%
%	Learned:
%	path(a,z).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%	path(A,B):-'$1'(A,B).
%	true.
%	==
%
%	Example 3
%	---------
%
%	This example demonstrates the use of the "combine" term in BK2.
%
%	The following call is as in Example 2, but this time the
%	definition of the single invented predicate '$1'/2 learned by
%	incremental refinement is added to the background knowledge
%	definition of edge/2, only.
%
%	==
%	?- _K = 1, _J = 0
%	,_BK2 = combine([edge/2])
%	,experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%	,learn_incremental([_Pos,_Neg,_BK,_MS,_K],[_Pos,_Neg,_BK2,_MS,_J],_Is,_Ps)
%	,print_clauses('Invented:',_Is), nl, print_clauses('Learned:',_Ps).
%	Invented:
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%
%	Learned:
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%	path(A,B):-'$1'(A,B).
%	true.
%	==
%
%	The following call passes combines the definition of '$1'/2 with
%	path/2 instead of edge/2:
%
%	==
%	?- _K = 1, _J = 0
%	,_BK2 = combine([path/2])
%	,experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%	,learn_incremental([_Pos,_Neg,_BK,_MS,_K],[_Pos,_Neg,_BK2,_MS,_J],_Is,_Ps)
%	,print_clauses('Invented:',_Is), nl, print_clauses('Learned:',_Ps).
%	Invented:
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%
%	Learned:
%	path(a,z).
%	'$1'(A,B):-path(A,B).
%	path(A,B):-'$1'(A,B).
%	true.
%	==
%
%	Example 4
%	---------
%
%	This example demonstrates the use of a different set of
%	metarules in the second stage of learning with incremental
%	refinement.
%
%	The call below is similar to the call in Example 1 in that
%	"invented" is passed as the value of BK2, however in
%	this call, learning in the first stage uses the set of metarules
%	whose identifiers are given in _MS1, i.e. Chain and Identity,
%	whereas learning in the second stage uses on Chain.:
%
%	==
%	?- _K = 1, _J = 0
%	,_BK2 = invented
%	,_MS1 = [chain,identity], _MS2 = [chain]
%	,experiment_data(path/2,_Pos,_Neg,_BK,_)
%	,learn_incremental([_Pos,_Neg,_BK,_MS1,_K],[_Pos,_Neg,_BK2,_MS2,_J],_Is,_Ps)
%	,print_clauses('Invented:',_Is), nl, print_clauses('Learned:',_Ps).
%	Invented:
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%
%	Learned:
%	path(a,z).
%	true.
%	==
%
%	Learning in the second stage is not possible given only Chain
%	and '$1'/2. Increasing the number of predicates invented in the
%	first stage does the trick:
%
%	==
%	?- _K = 4, _J = 0
%	,_BK2 = invented
%	,_MS1 = [chain,identity], _MS2 = [chain]
%	,experiment_data(path/2,_Pos,_Neg,_BK,_)
%	,learn_incremental([_Pos,_Neg,_BK,_MS1,_K],[_Pos,_Neg,_BK2,_MS2,_J],_Is,_Ps)
%	,print_clauses('Invented:',_Is), nl, print_clauses('Learned:',_Ps).
%	Invented:
%	'$4'(A,B):-'$3'(A,B).
%	'$4'(A,B):-'$3'(A,C),'$3'(C,B).
%	'$3'(A,B):-'$2'(A,B).
%	'$3'(A,B):-'$2'(A,C),'$2'(C,B).
%	'$2'(A,B):-'$1'(A,B).
%	'$2'(A,B):-'$1'(A,C),'$1'(C,B).
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%
%	Learned:
%	path(a,z).
%	'$2'(A,B):-'$1'(A,B).
%	'$3'(A,B):-'$2'(A,B).
%	'$4'(A,B):-'$3'(A,B).
%	'$2'(A,B):-'$1'(A,C),'$1'(C,B).
%	'$3'(A,B):-'$2'(A,C),'$2'(C,B).
%	'$4'(A,B):-'$3'(A,C),'$3'(C,B).
%	path(A,B):-'$4'(A,C),'$4'(C,B).
%	true.
%	==
%
%	Unfortunately it's difficult to determine the necessary number
%	of invented predicates beforehand for arbitrary programs. A
%	first step of experimentation is recommended, where incremental
%	refinement is essentially used for knowledge discovery.
%
%
learn_incremental([Pos1,Neg1,BK1,MS1,K1],[Pos2,Neg2,BK2,MS2,K2],Is,Ps):-
	incremental_refinement(K1,Pos1,Neg1,BK1,MS1,Is)
	,(   BK2 = combine(BK2_)
	->   new_symbols(Is,Ss)
	    ,append(BK2_,Ss,BK_F)
	 ;   BK2 = append
	 ->  new_symbols(Is,Ss)
	    ,append(BK1,Ss,BK_F)
	 ;   BK2 = invented
	 ->  new_symbols(Is,BK_F)
	 ;   BK_F = BK2
	 )
	,S = (assert_program(user,Is,Rs)
	     )
	,G = (auxiliaries:set_configuration_option(max_invented, [K2])
	     ,learn_dynamic(Pos2,Neg2,BK_F,MS2,Ps)
	     )
	,C = (erase_program_clauses(Rs)
	     )
	,setup_call_cleanup(S,G,C).



%!      learn_incremental(+Pos,+Neg,+BK,+Metarules,-Program) is det.
%
%       Learn a Program by incremental refinement of a theory.
%
%	As learn_incremental/4 but a) the first and second stage of
%	learning Program are performed with the same elements of a MIL
%	problem, b) any predicates invented in the first stage are
%	added to the initial background knowledge before performing a
%	dynamic learning attempt and c) the number of predicates to
%	invent in the first stage is taken from the configuration option
%	max_invented/1.
%
%	The dynamic learning attempt in the second stage is performed
%	with the maximum number of invented predicates set to 0, i.e. no
%	new predicates are invented.
%
learn_incremental(Pos,Neg,BK,MS,Ps):-
	incremental_refinement(Pos,Neg,BK,MS,Is)
	,new_symbols(Is,Ss)
	,auxiliaries:set_configuration_option(max_invented, [0])
	,S = (assert_program(user,Is,Rs)
	     ,append(BK,Ss,BK_)
	     )
	,G = learn_dynamic(Pos,Neg,BK_,MS,Ps)
	,C = (erase_program_clauses(Rs)
	     )
	,setup_call_cleanup(S,G,C).



%!	incremental_refinement(+Pos,+Neg,+BK,+Metarules,-Invented) is
%!	det.
%
%       Perform predicate invention by incremental refinement of BK.
%
%	As incremental_refinement/6 but the number of predicates to be
%	invented is taken from the configuration option max_invented/1.
%
incremental_refinement(Pos,Neg,BK,MS,Ps):-
	configuration:max_invented(K)
	,incremental_refinement(K,Pos,Neg,BK,MS,Ps_)
	,flatten(Ps_,Ps).


%!	incremental_refinement(+K,+Pos,+Neg,+BK,+Mearules,-Invented) is
%!	det.
%
%	Perform predicate invnetion by incremental refinement of BK.
%
%	K is an integer, the maximum number of predicates to invent.
%	Pos, Neg, BK and Metarules are the elements of MIL problem.
%	Invented is a set of clauses of the definitions of one or more
%	invented predicates.
%
%	The clauses of invented are derived by the following algorithm:
%
%	==
%	Proc REFINE(Pos,Neg,BK,Metarules)
%	INITIALISE:
%	Set current iteration i	= 0
%	Set Pos_g = generalise_examples(Pos)
%	Set Invented = []
%	1. WHILE i =< K
%	2.   i++
%	3.   $i <- learn(Pos_g,Neg,BK,Metarules)
%	4.   Invented <- Invented U {$i}
%	5.   BK <- {$i}
%	6.   REFINE(Pos,Neg,BK,Metarules)
%	7. RETURN Invented
%	==
%
%	In plain English, first the positive examples in Pos are
%	generalised by variabilising the last argument of each example.
%	generalise_examples/1 also replaces the predicate symbol of each
%	positive example with a new, invented predicate symbol. Then, an
%	attempt is made to learn a hypothesis from the elements of the
%	MIL problem given in the input. If the attempt succeeds, the
%	hypothesis learned in this way replaces all definitions in the
%	background knowledge and a new learning attempt is made. The
%	process continues for K iterations or until a learning attempt
%	fails, at which point the hypotheses learned until that
%	iteration are returned as a list of clauses.
%
%	The following examples all use the elements of the path/2 MIL
%	problem defined in data/examples/incremental_refinemnt.pl,
%	described in the examples section of learn_incremental/4.
%
%	==
%	?- _K = 1
%	,experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%	,incremental_refinement(_K,_Pos,_Neg,_BK,_MS,_Is)
%	,print_clauses(_Is).
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%	true.
%
%	?- _K = 2, experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%	,incremental_refinement(_K,_Pos,_Neg,_BK,_MS,_Is)
%	,print_clauses(_Is).
%	'$2'(A,B):-'$1'(A,B).
%	'$2'(A,B):-'$1'(A,C),'$1'(C,B).
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%	true.
%
%	?- _K = 3
%	,experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%	,incremental_refinement(_K,_Pos,_Neg,_BK,_MS,_Is),print_clauses(_Is).
%	'$3'(A,B):-'$2'(A,B).
%	'$3'(A,B):-'$2'(A,C),'$2'(C,B).
%	'$2'(A,B):-'$1'(A,B).
%	'$2'(A,B):-'$1'(A,C),'$1'(C,B).
%	'$1'(A,B):-edge(A,B).
%	'$1'(A,B):-path(A,B).
%	'$1'(A,B):-edge(A,C),edge(C,B).
%	true.
%	==
%
incremental_refinement(K,Pos,Neg,BK,MS,Ps):-
	incremental_refinement(K,0,Pos,Neg,BK,MS,[],Ps_,[])
	,flatten(Ps_,Ps).


%!      incremental_refinement(+I,+Pos,+Neg,+BK,+MS,+Acc_Is,-Is,+Acc_Rs)
%!       is det.
%
%       Business end of incremental_refinement/5.
%
incremental_refinement(K,I,Pos,Neg,BK,MS,Acc_Ps,Bind_Ps,Acc_Rs):-
	succ(I,I_)
	,generalise_examples(K,I_,Pos,Pos_)
	,learn(Pos_,Neg,BK,MS,[_|Ps])
	,assert_program(user,Ps,Rs)
	,new_symbols(Ps,Ss)
	,dynamic_learning:table_encapsulated(Ss)
	,!
	,incremental_refinement(K,I_,Pos_,Neg,Ss,MS,[Ps|Acc_Ps],Bind_Ps,[Rs|Acc_Rs]).
incremental_refinement(_K,_I,_Pos,_Neg,BK,_MS,Ps,Ps,Acc_Rs):-
	flatten(Acc_Rs,Fs)
	,erase_program_clauses(Fs)
	,dynamic_learning:untable_encapsulated(BK).


%!      generalise_examples(+I,+Examples,-Generalised) is det.
%
%       Generalise a set of positive Examples.
%
generalise_examples(K,I,Es,Es_):-
	I =< K
	,findall(E_
	       ,(member(E,Es)
		,generalise_example(I,E,E_)
		)
	       ,Es_).


%!      generalise_example(+I,+Example,-Generalised) is det.
%
%       Generalise a positive example.
%
generalise_example(I,E,E_):-
	E =.. [_F,A,_B]
	,atom_concat('$',I,F_)
	,E_ =.. [F_,A,_].


%!      new_symbols(+Clauses,-Symbols) is det.
%
%       Collect the Symbols of a list of invented predicates Clauses.
%
new_symbols(Cs,Ss):-
	setof(F/A
	     ,H^B^Cs^(member(H:-B,Cs)
	      ,functor(H,F,A)
	      )
	     ,Ss).

