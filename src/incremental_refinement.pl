:-module(incremental_refinement, [learn_incremental/3
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


%!      learn_incremental(+Problem_1,+Problem_2,-Program) is det.
%
%       Learn a Program by incremental refinement of a theory.
%
%       Problem_1 and Problem_2 are lists [P,N,B,M,K] each representing
%       a MIL problem, where P is a set of positive examples, N is a set
%       of negative examples, B is a set of predicate symbols and
%       arities of background predicates, M is a set of atomic metarule
%       identifiers and K an integer, the maximum number of predicates
%       to invent.
%
%       Program is a program learned in two stages from Problem_1 and
%       Problem_2. In the first stage, the background knowledge, B, in
%       Problem_1 is incrementally refined by incremental_refinement/5.
%       In the secont stage, the refined background knowledge is added
%       to, or replaces the background knowledge in Problem_2 and
%       Program is learned by dynamic learning.
%
%       The background knowledge, B, in Problem_2 can include the atom
%       "append". In that case, the predicate symbols of any predicates
%       invented in the first stage will be appended to the symbols in
%       B.
%
%       Examples of calls
%       -----------------
%
%	The examples below use the elements of the MIL problem defined
%	in data/examples/incremental_refinemnt.pl. The MIL problem
%	includes a single example of a target predicate path/2 and
%	background knowledge consisting of atoms of edge/2.
%
%       The following call performs incremental refinement of the
%       background knowledge given for path/2 and an invented predicate
%       limit of 5. Five predicates are invented with predicate symbols
%       '$1'/2 to '$5'/2. These are appended to the original background
%       knowledge for path/2 and a new learning attempt is made, with an
%       invented predicate limit of 0 (so no new predicates are
%       invented). The result is a hypothesis that uses all five
%       predicates invented in the incremental refinement step.
%
%       ==
%       ?- experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%       ,learn_incremental([_Pos,_Neg,_BK,_MS,5],[_Pos,_Neg,[append|_BK],_MS,0],_Ps)
%       ,print_clauses(_Ps).
%       path(a,z).
%       '$1'(A,B):-edge(A,B).
%       '$1'(A,B):-edge(A,C),edge(C,B).
%       '$2'(A,B):-'$1'(A,B).
%       '$2'(A,B):-'$1'(A,C),'$1'(C,B).
%       '$3'(A,B):-'$2'(A,B).
%       '$3'(A,B):-'$2'(A,C),'$2'(C,B).
%       '$4'(A,B):-'$3'(A,B).
%       '$4'(A,B):-'$3'(A,C),'$3'(C,B).
%       '$5'(A,B):-'$4'(A,B).
%       '$5'(A,B):-'$4'(A,C),'$4'(C,B).
%       path(A,B):-'$5'(A,B).
%       path(A,B):-edge(A,C),path(C,B).
%       path(A,B):-'$5'(A,C),'$5'(C,B).
%       true.
%       ==
%
%	The following call performs incremental refinement of the
%	background knowledge for path/2 with an invented predicate limit
%	of 4. In the next stage, a new learning attempt is made
%	appending all invented predicates to the initial background
%	knowledge and with an invented predicate limit of 1, so one more
%	predicate is invented during this learning attempt and used to
%	construct the final hypothesis. This new invented predicate is
%	named '$1'/2, as one of the predicates invented in the first
%	step (the counters for invented predicates' names are not
%	transferred between incremental refinement and dynamic learning,
%	however the new clauses complete the definition of '$1'/2).
%
%	This example demonstrates the combined use of incremental
%	refinement and dynamic learning. Note that dynamic learning can
%	also learn a definition of path/2 by inventing component
%	predicates but currently this takes a lot longer because of
%	inefficiencies in the implementation.
%
%       ==
%       ?- experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%       ,learn_incremental([_Pos,_Neg,_BK,_MS,4],[_Pos,_Neg,[append|_BK],_MS,1],_Ps)
%       ,print_clauses(_Ps).
%       path(a,z).
%       '$1'(A,B):-edge(A,B).
%       '$1'(A,B):-edge(A,C),edge(C,B).
%       '$2'(A,B):-'$1'(A,B).
%       '$3'(A,B):-'$2'(A,B).
%       '$4'(A,B):-'$3'(A,B).
%       '$4'(A,B):-'$3'(A,C),'$3'(C,B).
%       '$1'(A,B):-path(A,C),path(C,B).
%       '$1'(A,B):-path(A,C),edge(C,B).
%       '$1'(A,B):-path(A,C),'$4'(C,B).
%       '$1'(A,B):-edge(A,C),'$1'(C,B).
%       '$1'(A,B):-path(A,C),'$1'(C,B).
%       '$1'(A,B):-'$3'(A,C),'$4'(C,B).
%       '$1'(A,B):-'$4'(A,C),'$4'(C,B).
%       path(A,B):-'$3'(A,B).
%       path(A,B):-'$4'(A,B).
%       path(A,B):-'$4'(A,C),'$4'(C,B).
%       true.
%       ==
%
%       The following call performs incremental refinement of the
%       background knowledge for path/2 with an invented predicate limit
%       of 5. In the next stage, a new learning attempt is made
%       replacing background knowledge with the invented predicate '$'/5
%       and the initial metarules (including chain and identity) with
%       chain only and with an invented predicate limit of 0.
%
%	This example demonstrates a more complex use of learning with
%	incremental refinement that would normally require some
%	exploration of the program, e.g. before the user has the
%	certainty to choose only a subset of the predicates invented in
%	the incremental refinement stage and a different set of
%	metarules etc. Although learn_incremental/[3,5] can be used
%	directly in this way, it is expected that a user will first
%	conduct a few experiments with incremental_refinement/5 to find
%	out "what works" for a particular dataset and whether
%	incremental refinement is useful in the first place.
%
%       ==
%       ?- experiment_data(path/2,_Pos,_Neg,_BK,_MS)
%       ,learn_incremental([_Pos,_Neg,_BK,_MS,5],[_Pos,_Neg,['$5'/2],[chain],0],_Ps)
%       ,print_clauses(_Ps).
%       path(a,z).
%       path(A,B):-'$5'(A,C),'$5'(C,B).
%       true.
%       ==
%
%
learn_incremental([Pos1,Neg1,BK1,MS1,K1],[Pos2,Neg2,BK2,MS2,K2],Ps):-
	%auxiliaries:set_configuration_option(max_invented, [K1])
	incremental_refinement(K1,Pos1,Neg1,BK1,MS1,Is)
	,(   selectchk(append,BK2,BK2_1)
	->   new_symbols(Is,Ss)
	    ,append(BK2_1,Ss,BK_F)
	 ;   BK_F = BK2
	 )
	,S = (assert_program(user,Is,Rs)
	     )
	,G = (%auxiliaries:set_configuration_option(max_invented, [K2])
	     learn_dynamic(K2,Pos2,Neg2,BK_F,MS2,Ps)
	     )
	,C = (erase_program_clauses(Rs)
	     )
	,setup_call_cleanup(S,G,C).



%!      learn_incremental(+Pos,+Neg,+BK,+Metarules,-Program) is det.
%
%       Learn a Program by incremental refinement of a theory.
%
%	As learn_incremental/3 but a) the first and second stage of
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

