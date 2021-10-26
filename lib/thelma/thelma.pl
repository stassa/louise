:-module(thelma, [thelma/1
                 ,thelma/2
                 ,thelma/5
                 ]).


/** <module>. THEory Learning MAchine - an implementation of Metagol.

Predicates in this module implement Metagol, the original MIL algorithm,
based on a Prolog meta-interpreter modified to return the
meta-substitutions of metarules derived during resolution. The
implementation is called Thelma, as an acronym of THEory Learning
MAchine. Thelma is used as a predicate invention engine, an alternative
to Louise's own Dynamid Learning utility.

*/

%!      thelma(+Targets) is nondet.
%
%       Learn a program with Thelma and print out its clauses.
%
thelma(Ts):-
        thelma(Ts,Ps)
        ,print_clauses(Ps).


%!      thelma(+Targets,-Program) is nondet.
%
%       Learn a program with Thelma.
%
thelma(Ts,Ps):-
        tp_safe_experiment_data(Ts,Pos,Neg,BK,MS)
        ,thelma(Pos,Neg,BK,MS,Ps).



%!      thelma(+Pos,+Neg,+BK,+MS,-Program) is nondet.
%
%       Learn a Program with Thelma.
%
thelma(Pos,Neg,BK,MS,Ps):-
        debug(thelma,'Encapsulating problem...',[])
        ,encapsulated_problem(Pos,Neg,BK,MS,[Pos_,Neg_,BK_,MS_])
        ,examples_targets(Pos,Ts)
        ,S = (debug(thelma,'Writing program...',[])
             ,write_program(thelma,Pos_,BK_,Refs)
             )
        ,G = (debug(thelma,'Proving examples...',[])
             % Note we're passing down the BK _symbols_ not clauses.
             % The symbols are used to identify BK atoms.
             % The (encapsulated) clauses are written to the database
             % and used in resolution-refutation of examples.
             ,prove(Ts,Pos_,BK,MS_,Ms)
             ,debug(thelma,'Applying metarules...',[])
             ,applied_metarules(Ms,MS,Ms_)
             ,debug_clauses(thelma,'Current hypothesis:',Ms_)
             ,debug(thelma,'Disproving examples...',[])
             ,disprove(Neg_,Ms_)
             )
        ,Cl = (debug(thelma,'Erasing program clauses...',[])
              ,erase_program_clauses(Refs))
        ,setup_call_cleanup(S,G,Cl)
        ,debug(thelma,'Excapsulating program...',[])
        ,excapsulated_clauses(Ts,Ms_,Ps).


%!	depth_level(+Clause_Max,+Invented_Max,-Clauses,-Invented) is
%!	nondet.
%
%	Increment the search depth level.
%
%	Clause_Max, Invented_Max, Clauses and Invented are integers.
%	Clauses and Invented increase by one on backtracking. Clauses
%	ranges from 1 to Clauses_Max. Invented ranges from 0 to
%	Invented_Max or Clause-Max - 1, whichever is lower.
%
%	Explanation
%	===========
%
%	Thelma performs an iterative deepening search for a _hypothesis_
%	that explains its training examples.
%
%	A hypothesis is a list of definite datalog clauses that may
%	include definitions of one or more invented predicates. The
%	depth of the search at each iteration corresponds to the _size_
%	of the hypothesis, defined as the number of all clauses in the
%	hypothesis.
%
%	This predicate is responsible for controlling the depth of the
%	search by incrementing the values of Clauses and Invented.
%
%	Clauses is the maximum hypothesis size at the current search
%	depth. Invented is the maximum number of clauses in all invented
%	definitions in the hypothesis at the current search depth.
%
%	Clause_Max and Invented_Max are the maximum number of all
%	clauses in the hypothesis and the maximum number of all clauses
%	of invented predicates in the hypothesis, in the entire search.
%
%	Clauses and Invented are first bound to Clauses=1, Invented=0
%	and then to that number plus 1 on each successive backtracking
%	into depth_level/4. The search exits without further
%	backtracking when Clauses is equal to Clause_Max and Invented
%	equal to Invented_Max, or when no more hypotheses can be found.
%
depth_level(1,0,1,0):-
	!.
depth_level(C,I,C_,I_):-
	between(1,C,C_)
	,between(0,I,I_)
	,I_ < C_.



%!	write_program(+Module,+Pos,+BK,+PS,-Refs) is det.
%
%	Write an encapsulated MIL problem to a module.
%
%	@tbd The negative examples and metarules don't need to be
%	written to the dynamic database.
%
write_program(M,Pos,BK,Rs):-
	findall(Rs_i
		,(member(P, [Pos,BK])
		 ,assert_program(M,P,Rs_i)
		 )
		,Rs_)
	,flatten(Rs_,Rs).


%!      prove(+Symbols,+Atoms,+BK,+Metarules,-Clauses) is nondet.
%
%       Prove a set of Atoms and induce a set of Clauses.
%
prove(Ts,As,BK,MS,Ss):-
        configuration:depth_limits(C,I)
        ,depth_level(C,I,K,J)
        ,debug(depth_level,'Depth level: ~w clauses, ~w invented.',[K,J])
        ,predicate_signature(Ts,J,Sig)
        ,prove(K,As,BK,MS,Sig,[],Ss).


%!      prove(+K,+Atoms,+BK,+Metarules,+Sig,+Acc,-Clauses) is nondet.
%
%       Business end of prove/5.
%
%       +K is an upper bound to the cardinality of the learned set of
%       Clauses.
%
prove(_K,[],_BK,_MS,_Sig,Ps,Ps):-
        !
       ,debug_clauses(prove,'Proved program:',Ps).
prove(K,[A|As],BK,MS,Sig,Acc,Bind):-
        debug(prove,'Proving atom (BK): ~w', [A])
        ,background_predicate(A,BK)
        ,!
        ,prove_bk_atom(A)
        ,prove(K,As,BK,MS,Sig,Acc,Bind).
prove(K,[A|As],BK,MS,Sig,Acc1,Bind):-
        debug(prove,'Proving atom (select metasub): ~w', [A])
        ,select_metasub(Acc1,MS,A,Bs)
        ,prove(K,Bs,BK,MS,Sig,Acc1,Acc2)
        % Red hot cut. Cuts unnecessary backtracking- but what else besides?
        ,!
        ,debug(prove, 'Proving remaining atoms: ~w', [As])
        ,prove(K,As,BK,MS,Sig,Acc2,Bind).
prove(K,[A|As],BK,MS,Sig,Acc1,Bind):-
        debug(prove,'Proving atom (new metasub): ~w', [A])
        ,new_metasub(K,Acc1,A,MS,Sig,Acc2,Bs)
        ,prove(K,Bs,BK,MS,Sig,Acc2,Acc3)
        ,debug(prove, 'Proving remaining atoms: ~w', [As])
        ,prove(K,As,BK,MS,Sig,Acc3,Bind).


%!      background_predicate(+Atom,+Symbols) is det.
%
%       True when Atom is an atom of a predicate in Symbols.
%
background_predicate(A,BK):-
        A =.. [m|[S|As]]
        ,length(As, N)
        ,memberchk(S/N, BK)
        ,debug(bk_atom,'Background atom: ~w', [A]).


%!      prove_bk_atom(+Atom) is nondet.
%
%       Prove a BK atom.
%
%       Atom is an encapsulated atom of a predicate in a background
%       definition. Refutation-proof of Atom is left to Prolog's engine
%       via call/1.
%
prove_bk_atom(A):-
        call(thelma:A)
        ,debug(prove_bk_atom,'Proof succeeded: ~w',[A]).


%!      select_metasub(+Metasubs,+Atom,-Body) is nondet.
%
%       Select an existing metasubstitution to refute an Atom.
%
%       Metasubs is a list of metasubstitutions Sub-M where Sub is a
%       metasubstitution atom and M is an encapsulated metarule.
%
%       Atom is an atom bound to the head of a metarule M in a pair
%       Sub-M selected from Metasubs and later refuted by resolution
%       of the body atoms of M with BK and metarules in prove/7.
%
%       Body is the list of literals in the body of the selected
%       metarule M as above. Each atom of Body is added to the stack of
%       atoms to be refuted in prove/7.
%
select_metasub(Ss,_MS,A,Ls):-
        debug(select_metasub,'Selecting known metasub to prove atom: ~w',[A])
        ,member(Sub-M,Ss)
        % Make a copy to avoid binding variables in all instances of the metasub.
        ,copy_term(Sub-M, Sub_-M_)
        ,louise:bind_head_literal(A,M_,(Sub_:-(H,Ls_)))
        ,once(list_tree(Ls,Ls_))
        ,debug(select_metasub,'Selected metasub atom: ~w',[Sub_])
        ,debug(select_metasub,'Selected metasub literals: ~w',[H:-Ls_]).


%!      new_metasub(+K,+Metasubs,+Atom,+MS,+Sig,-New,-Body) is nondet.
%
%       Create a new metasubstitution to refute an Atom.
%
%       K is an upper bound on the new metasubstitutions that can be
%       created and added to a hypothesis.
%
%       Metasubs is a list of metasubstitutions Sub-M where Sub is a
%       metasubstitution atom and M is an encapsulated metarule.
%
%       Atom is an atom bound to the head of a metarule M selected from
%       MS and later refuted by resolution of the body atoms of M with
%       BK and metarules in prove/7.
%
%       MS is the set of metarules in the current learning problem.
%
%       Sig is the predicate signature, used to instantiate second-order
%       variables in the selected metarule M.
%
%       New is the list Metasubs with a new pair Sub-M added to it. Sub
%       is the metasubstitution atom of the metarule M selected to
%       refute Atom, as described above.
%
%       Body is the list of literals in the body of the selected
%       metarule M as above. Each atom of Body is added to the stack of
%       atoms to be refuted in prove/7.
%
new_metasub(K,Ss,A,MS,Sig,[Sub-M|Ss],Ls):-
        debug(new_metasub,'Creating new metasub to prove atom: ~w',[A])
        ,length(Ss,N)
        %,writeln('You are here (0)':N<K)
        ,N < K
        %,writeln('You are here (1)')
        ,member(M,MS)
        % Make a copy to avoid binding variables in all instances of M.
        ,copy_term(M, M_)
        %,writeln('You are here (2)')
        ,debug_quantified_metarules(new_metasub,'Selected metarule:',[M_])
        % Note that variables are bound in the copy.
        % The original instance is bound to the output in [Sub-M|Ss].
        %,writeln('You are here (3)')
        ,louise:bind_head_literal(A,M_,(Sub:-(H,Ls_)))
        %,writeln('You are here (4)')
        ,bind_existential(Sub,Sig)
        ,once(list_tree(Ls,Ls_))
        ,debug(new_metasub,'New metasub atom: ~w',[Sub])
        ,debug(new_metasub,'New metasub literals: ~w',[H:-Ls_]).


%!      bind_existential(+Metasub,+Signature) is nondet.
%
%       Bind existentially quantified variables in a Metasub atom.
%
%       Metasub is an encapsulated metasubstitution atom m(ID,P,Q,R,...)
%       where ID is the name of a metarule, P is the symbol of a target
%       predicate and Q, R, ... are the symbols of predicates in
%       literals in the body of the metarule with the given ID.
%
%       Signature is the predicate signature, a list of predicate
%       symbols and arities of predicates in the positive examples,
%       background knowledge and any invented predicates.
%
%       This predicate instantiates each of P,Q,R... to a symbol in
%       Signature.
%
bind_existential(Sub,Sig):-
        debug(bind_existential,'Binding existential variables: ~w',[Sub])
        ,Sub =.. [m|[_Id,_T|Es]]
        ,bind_existential_(Es,Sig,Es).

%!      bind_existential_(+Sub,+Sig) is nondet.
%
%       Business end of bind_existential/2.
%
bind_existential_([],_Sig,_Acc).
% I know, weird. Acc stays unbound and yet it doesn't.
bind_existential_([S|Sub],Sig,Acc):-
        member(S/_,Sig)
        ,bind_existential_(Sub,Sig,Acc).



%!	disprove(+Atoms,+Program) is det.
%
%	True when a Program does not cover negative examples.
%
%	Atoms is a list of negative examples of the learning target. It
%	is a list of lists where each sublist is an atom in the form of
%	a list [F|As], where F the symbol of the target predicate and As
%	the list of the atom's terms.
%
%	Program is a list of definite datalog clauses, a hypothesis
%	formed by a call to prove/6.
%
%	disprove/2 fails iff an atom in Atoms is entailed by Program.
%	This is tested by first asserting Program to the dynamic
%	database and then proving each atom in Atoms with call/1.
%	Program is retracted from the database after the proof
%	completes.
%
disprove([],_Ms):-
% Skip further processing if there are no negative examples.
	!.
disprove(Neg,Ps):-
        S  = assert_program(thelma,Ps,Refs)
        % Succeed if the program fails, otherwise fail;
	% Erase the newly asserted clauses eitherwise.
        ,G = forall(member(:-A,Neg)
                    ,(debug(thelma,'Disproving atom: ~w',[:-A])
                     ,\+ once(call(thelma:A))
                     )
                   )
        ,C = erase_program_clauses(Refs)
        ,setup_call_cleanup(S,G,C).
