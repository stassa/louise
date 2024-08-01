:-module(spherical_scientist, [learn_reduced/4
                              ,counter_examples/3
                              ,hypothesis_predicates/3
                              ,hypothesis_actions/3
                              ,hypothesis_tuples/3
                              ,hypothesis_tuples/4
                              ]).

:-use_module(src(louise)).
:-use_module(src(auxiliaries)).
:-use_module(src(mil_problem)).
:-use_module(src(subsystems/greedy),[]).
%:-use_module(controller_freak).

/** <module> An idealised spherical scientist in a perfect, frictionless vacuum.

Generate counter-examples of a Hypothesis from the generalisation of an
initial observation:
==
?- _M = load_unload, _T = s/2, program([_T],_M,_Ps), _M:positive_example(_T,_E), counter_examples(_E,_Ps,_Ns), length(_Ns,N).
N = 0.
==

Collect background predicates used in a hypothesis:
==
?- program([s/2],pomdp_fsc,_Ps),_E = s([q0,[a,-,-,-,b,-,-,-,a],[right,right,right,right,left,left,left,left,left]],[q1,[],[]]), hypothesis_predicates(_Ps,_E,_Rs), maplist(writeln,_Rs).
t0/2
t1/2
t11/2
t17/2
t18/2
t19/2
t2/2
t21/2
t23/2
t5/2
t8/2
t9/2
true.
==

Collect the tuples in a hypothesis (saved in a file):
==
?- _T = s/2, _M = load_unload_fsc, program([_T],_M,_Ps), _M:positive_example(_T,_E), hypothesis_tuples(_Ps,_E,_Ts), print_clauses(_Ts).
t0(q0,-,right,q0).
t1(q0,-,left,q0).
t14(q0,a,stop,q1).
t3(q0,a,right,q0).
t7(q0,b,left,q0).
true.
==

Collect the Prolog predicates corresponding to tuples in a hypothesis
(saved in a file):
==
?- _T = s/2, _M = load_unload_fsc, program([_T],_M,_Ps),
_M:positive_example(_T,_E), hypothesis_actions(_Ps,_E,_Ts),
print_clauses(_Ts). t0([[q0|A],[-|B],[right|C],[q0|D]],[A,B,C,D]).
t1([[q0|A],[-|B],[left|C],[q0|D]],[A,B,C,D]).
t14([[q0|A],[a|B],[stop|C],[q1|D]],[A,B,C,D]).
t3([[q0|A],[a|B],[right|C],[q0|D]],[A,B,C,D]).
t7([[q0|A],[b|B],[left|C],[q0|D]],[A,B,C,D]).
true.
==


Learn a hypothesis reducing the first-order theory and deriving
counter-examples on the way:
==
?- experiment_data(fsc/2,_Pos,_Neg,_BK,_MS), spherical_scientist:learn_reduced(_Pos,_BK,_MS,_Ps), member(_Cs,_Ps), print_clauses(_Cs), length(_Cs,N).
% Learning initial hypothesis...
% Initial hypothesis:
% s([q0,[a,-,-,-,b,-,-,-,a],[right,right,right,right,left,left,left,left,left]],[q1,[],[]])
% s(A,B):-t21(A,B)
% s(A,B):-t9(A,B)
% s(A,B):-t0(A,C),s(C,B)
% s(A,B):-t1(A,C),s(C,B)
% s(A,B):-t11(A,C),s(C,B)
% s(A,B):-t17(A,C),s(C,B)
% s(A,B):-t18(A,C),s(C,B)
% s(A,B):-t19(A,C),s(C,B)
% s(A,B):-t2(A,C),s(C,B)
% s(A,B):-t23(A,C),s(C,B)
% s(A,B):-t5(A,C),s(C,B)
% s(A,B):-t8(A,C),s(C,B)
% (13 clauses)
% Reducing problem...
% Derived 63 counter-examples
% Kept 12 predicates: [t0/2,t1/2,t11/2,t17/2,t18/2,t19/2,t2/2,t21/2,t23/2,t5/2,t8/2,t9/2]
% Learning reduced hypothesis...
% Reduced hypothesis ~w:
% s([q0,[a,-,-,-,b,-,-,-,a],[right,right,right,right,left,left,left,left,left]],[q1,[],[]])
% s(A,B):-t21(A,B)
% s(A,B):-t9(A,B)
% s(A,B):-t0(A,C),s(C,B)
% s(A,B):-t1(A,C),s(C,B)
% s(A,B):-t11(A,C),s(C,B)
% s(A,B):-t17(A,C),s(C,B)
% s(A,B):-t18(A,C),s(C,B)
% s(A,B):-t19(A,C),s(C,B)
% s(A,B):-t2(A,C),s(C,B)
% s(A,B):-t8(A,C),s(C,B)
% (11 clauses)
% Reducing problem...
% Derived 63 counter-examples
% Kept 10 predicates: [t0/2,t1/2,t11/2,t17/2,t18/2,t19/2,t2/2,t21/2,t8/2,t9/2]
% Learning reduced hypothesis...
% Learning final sub-hypotheses...
s(A,B):-t9(A,B).
s(A,B):-t1(A,C),s(C,B).
s(A,B):-t17(A,C),s(C,B).
s(A,B):-t18(A,C),s(C,B).
s(A,B):-t8(A,C),s(C,B).
N = 5 ;
s(A,B):-t21(A,B).
s(A,B):-t0(A,C),s(C,B).
s(A,B):-t11(A,C),s(C,B).
s(A,B):-t19(A,C),s(C,B).
s(A,B):-t2(A,C),s(C,B).
N = 5.
==


*/

%!      learn_reduced(+Observation,+FOT,+SOT,-Hypothesis) is det.
%
%       Learn a reduced Hypothesis.
%
learn_reduced([O],BK,MS,Ps):-
        debug(learn_reduced,'Learning initial hypothesis...',[])
        ,learn([O],[],BK,MS,Ps_)
        ,length(Ps_,N)
        ,debug_clauses(learn_reduced,'Initial hypothesis:',Ps_)
        ,debug(learn_reduced,'(~w clauses)',[N])
        ,reduced_problem(Ps_,O,Cs,BK_)
        ,learn_reduced(N,O,Cs,BK_,MS,_Acc,Ps).


%!      learn_reduced(+N,+Observation,+Neg,+BK,+MS,+Acc,-Prog)
%       is nondet.
%
%       Business end of learn_reduced/4.
%
learn_reduced(N,O,Neg,BK,MS,Acc,Ps):-
        debug(learn_reduced,'Learning reduced hypothesis...',[])
        ,learn([O],Neg,BK,MS,Ps_)
        ,length(Ps_,M)
        ,M < N
        ,!
        ,debug_clauses(learn_reduced,'Reduced hypothesis:',Ps_)
        ,debug(learn_reduced,'(~w clauses)',[M])
        ,reduced_problem(Ps_,O,Cs,BK_)
        ,learn_reduced(M,O,Cs,BK_,MS,Acc,Ps).
learn_reduced(_N,O,Neg,BK,MS,_,Ps):-
        debug(learn_reduced,'Learning final sub-hypotheses...',[])
        ,findall(Ps
               %,(learn_greedy([O],Neg,BK,MS,Ps)
               ,(learn([O],Neg,BK,MS,Ps)
                ,Ps \= []
                )
               ,Ps)
        ,length(Ps,N)
        ,debug(learn_reduced,'Learned ~w sub-hypotheses:',[N]).


%!      reduced_problem(+Prog,+Observation,+Counter,+BK) is det.
%
%       Reduce a background theory and derive Counter examples.
%
reduced_problem(Ps,O,Cs,BK_):-
        debug(learn_reduced,'Reducing problem...',[])
        ,hypothesis_predicates(Ps,O,BK_)
        ,counter_examples(O,Ps,Cs)
        ,maplist(length,[Cs,BK_],[N,M])
        ,debug(learn_reduced,'Derived ~w counter-examples',[N])
        ,debug(learn_reduced,'Kept ~w predicates: ~w',[M,BK_]).



%!      counter_examples(+Observation,+Hypothesis,-Counters) is det.
%
%       Generate counter-examples of a Hypothesis.
%
%       @tbd Document.
%
counter_examples(E,Ps,Ns):-
        test_example(E,E_)
        ,debug(counter_examples,'Test example: ~w',[E_])
        ,examples_targets([E_],[T])
        ,S = (assert_program(experiment_file,Ps,Rs)
             ,table(experiment_file:T)
             )
        ,G = findall(:-E_
                    ,experiment_file:E_
                    ,Is)
        ,C = (erase_program_clauses(Rs)
             ,untable(experiment_file:T)
             )
        ,setup_call_cleanup(S,G,C)
        ,sort(Is,Ss)
        ,selectchk(:-E,Ss,Ns).


%!      test_example(+Example,-Test) is det.
%
%       Generalise an Observation to generate counter-examples.
%
/*
% Representation with list of tuples in states.
% The test example is the training example which has a generalised list
% of states.
test_example(_E,E):-
        experiment_file:positive_example(s/2,E).
*/
test_example(_E,E_):-
        experiment_file:positive_example(s/2,E)
        ,E =.. [s,S1,S2]
        ,S1 = [Q0s,Os,_As,Q1s]
        ,S1_ = [Q0s,Os,_,Q1s]
        ,E_ =.. [s,S1_,S2].


%!      hypothesis_predicates(+Program,+Instance,-Symbols) is det.
%
%       Collect the Symbols in a learned Program.
%
%       Used to reduce background predicates. Thin shell around
%       program_symbols/2.
%
hypothesis_predicates(Ps,O,Rs):-
        examples_targets([O],[T])
        ,program_symbols(Ps,Rs_)
        ,selectchk(T,Rs_,Rs).



%!      hypothesis_actions(+Program,+Example,-Actions) is det.
%
%       Collect Action predicates included in a Program.
%
hypothesis_actions(Ps,O,As):-
        hypothesis_predicates(Ps,O,Ss)
        ,findall(T
                ,(member(S/A,Ss)
                 ,functor(T,S,A)
                 ,experiment_file:T
                 )
                ,As).


%!      hypothesis_tuples(+Program,+Example,-Tuples) is det.
%
%       Collect transition Tuples for action predicates in a Program.
%
hypothesis_tuples(Ps,I,Ts):-
        hypothesis_predicates(Ps,I,Ss)
        ,findall(T
                ,(member(S/N,Ss)
                 ,functor(H,S,N)
                 ,experiment_file:H
                 %,H =.. [Ti,[S1,[O|Os],[A|As]],[S2,Os,As]]
                 ,H =.. [Ti,[[S1|Ss],[O|Os],[A|As],[S2|Ns]],[Ss,Os,As,Ns]]
                 ,T =.. [Ti,S1,O,A,S2]
                 )
                ,Ts).


hypothesis_tuples(Ps,Ts,I,Ts_):-
        hypothesis_predicates(Ps,I,Ss)
        ,findall(_
               ,(member(S/_N,Ss)
                ,functor(T,S,4)
                ,member(T,Ts)
                )
               ,Ts_).
