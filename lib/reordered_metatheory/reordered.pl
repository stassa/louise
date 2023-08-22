:-module(reordered, [prove_reordered/6
                    ,prove_reordered/4
                    ]).

:-use_module(src(mil_problem)).

/** <module> Reordered metatheory - reasoning for a life-long learning agent.

Example of use:
==
?- _Q = (p(X,Y)), _Bs = [ (p(X,Y):-q(X,Y)), (p(X,Y):-q(Y,X)), q(b,a)], _MS = [identity,inverse,chain,tailrec,precon,postcon,switch,swap], random_permutation(_MS,_MS_R), prove_reordered(_Q, _Bs, _MS_R, Rs).
X = b,
Y = a,
Rs = [identity, precon, swap, switch, postcon, inverse, tailrec, chain] ;
X = a,
Y = b,
Rs = [inverse, precon, identity, swap, switch, postcon, tailrec, chain] ;
false.
==

Complete documentation pending.

*/


%!      prove_reordered(+Min,+Max,+Query,+Theory,+Assumptions,-Reordered)
%!      is nondet
%
%       Prove a Query and reorder Assumptions by frequency of use.
%
%       As prove_reordered/4, but implements an iterative deepening search
%       over the size of the "assumption window", i.e. the subset of
%       the metarules in Assumptions that we consider each time.
%
prove_reordered(K,J,Q,Bs,MS,MS_R):-
        between(K,J,I)
        ,first_k(I,MS,MS_K)
        ,prove_reordered(Q,Bs,MS_K,MS_R).


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


%!      prove_reordered(+Query,+Theory,+Assumptions,-Reorderd) is det.
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
prove_reordered(Q,Bs,MS,MS_R):-
        expanded_metarules(MS,Es)
        ,maplist(encapsulated_clauses,[[Q],Bs],[[Q_],Bs_])
        ,S = (assert_program(reordered,Bs_,Rs)
             )
        ,G = prove_reorder(Q_,Es,MS_)
        ,C = (erase_program_clauses(Rs)
             )
        ,setup_call_cleanup(S,G,C)
        ,findall(Id
                ,(member(Sub:-_,MS_)
                 ,Sub =.. [m,Id|_]
                 )
                ,MS_R).


%!      prove_reorder(+Atoms,+Metarules,-Reordered) is nondet.
%
%       Reordering meta-interpreter. Business end of prove_reordered/4.
%
prove_reorder(true,MS,MS):-
        !.
prove_reorder((L,Ls),MS,Acc):-
        prove_reorder(L,MS,Acc1)
        ,prove_reorder(Ls,Acc1,Acc).
prove_reorder(L,MS,Acc):-
        L \= (_,_)
        ,select(M,MS,MS_)
        ,copy_term(M,M_)
        ,matching_clause(L,M_,B)
        ,prove_reorder(B,[M|MS_],Acc).
prove_reorder(L,MS,Acc):-
        L \= (_,_)
        ,clause(L,true)
        ,prove_reorder(true,MS,Acc).


%!      matching_clause(+Literal,+Metarule,-Body) is nondet.
%
%       Match a Metarule to a Literal and retrieve its Body.
%
%       Sounds macabre, eh?
%
matching_clause(L,(_Sub:-(L,B)),B):-
        clause(reordered:L,B).
matching_clause(L,(_Sub:-(L)),true):-
        clause(reordered:L,true).
