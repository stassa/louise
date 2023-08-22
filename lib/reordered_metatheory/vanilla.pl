:-module(vanilla, [prove_vanilla/6
                  ,prove_vanilla/4
                  ]).

:-use_module(src(mil_problem)).

/** <module> Vanilla - reasoning for a life-long learning agent.

*/


%!      prove_vanilla(+Min,+Max,+Query,+Theory,+Assumptions,-Reordered)
%!      is nondet
%
%       Prove a Query and reorder Assumptions by frequency of use.
%
%       As prove_vanilla/4, but implements an iterative deepening search
%       over the size of the "assumption window", i.e. the subset of
%       the metarules in Assumptions that we consider each time.
%
prove_vanilla(K,J,Q,Bs,MS,MS_R):-
        between(K,J,I)
        ,first_k(I,MS,MS_K)
        ,prove_vanilla(Q,Bs,MS_K,MS_R).


%!      first_k(?K,?List,?First_K) is det.
%
%       Select the first K elements of a List.
%
first_k(K,MS,MS_K):-
        length(MS_K,K)
        ,first_k(K,MS,[],MS_K)
        ,!.

%!      first_k(?K,?List,?Acc,?First_K) is semidet.
%
%       Business end of first_k/3.
%
first_k(K,[M|MS],Acc,[M|Bind]):-
        first_k(K,MS,Acc,Bind).
first_k(_K,_MS,MS_K,MS_K).


%!      prove_vanilla(+Query,+Theory,+Assumptions,-Reorderd) is det.
%
%       Prove a Query and reorder Assumptions by frequency of use.
%
%       Query is an atom to be proven by meta-interpertation.
%
%       Theory is a logic program by which to (dis) prove Query.
%
%       Assumptions is a set of metarules (given as IDs, to be
%       expanded). Query is proven only by clauses that match one or
%       more of the metarules in Assumptions.
%
%       Reordered is the set of metarules in Assumptions (rather, their
%       IDs) reordered according to their use in proving Query.
%
prove_vanilla(Q,Bs,MS,MS_R):-
        expanded_metarules(MS,Es)
        ,maplist(encapsulated_clauses,[[Q],Bs],[[Q_],Bs_])
        ,S = (assert_program(vanilla,Bs_,Rs)
             )
        ,G = vanilla(Q_,Es,MS_)
        ,C = (erase_program_clauses(Rs)
             )
        ,setup_call_cleanup(S,G,C)
        ,findall(Id
                ,(member(Sub:-_,MS_)
                 ,Sub =.. [m,Id|_]
                 )
                ,MS_R).


%!      vanilla(+Atoms,+Metarules,-Reordered) is nondet.
%
%       Reordering meta-interpreter. Business end of prove_vanilla/4.
%
vanilla(true,MS,MS):-
        !.
vanilla((L,Ls),MS,Acc):-
        vanilla(L,MS,Acc1)
        ,vanilla(Ls,Acc1,Acc).
vanilla(L,MS,Acc):-
        L \= (_,_)
        ,select(M,MS,MS_)
        ,copy_term(M,M_)
        ,matching_clause(L,M_,B)
        ,vanilla(B,[M|MS_],Acc).
vanilla(L,MS,Acc):-
        L \= (_,_)
        ,clause(L,true)
        ,vanilla(true,MS,Acc).


%!      matching_clause(+Literal,+Metarule,-Body) is nondet.
%
%       Match a Metarule to a Literal and retrieve its Body.
%
%       Sounds macabre, eh?
%
matching_clause(L,(_Sub:-(L,B)),B):-
        clause(vanilla:L,B).
matching_clause(L,(_Sub:-(L)),true):-
        clause(vanilla:L,true).
