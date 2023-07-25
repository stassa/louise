:-module(compare_reduced, [compare_reduced/4
                          ]).

:-debug(compare_reduced).

/** <module> Code to test the Reduction Mystery.
*/

:-use_module(src(auxiliaries)).
:-use_module(src(louise)).
:-use_module(src(mil_problem)).
:-use_module(lib(evaluation/evaluation)).

/*
% Anti-recursion constraint - excludes recursive clauses
% Does not take into account invented or metarules with existentially
% quantified secod-order variables.
%
% Uncomment this to remove recursive clauses causing atomic residue to
% act as a "bridge" to cover more testing examples. See notes in
% compare_reduced/4 below.
%
configuration:metarule_constraints(m(tailrec,_,_),fail).
configuration:metarule_constraints(M,fail):-
	M =.. [m,Id,P|Ps]
        ,\+ memberchk(Id,[abduce
			 ,unit
			 ,projection_21
			 ,projection_12])
	,memberchk(P,Ps).
*/


%!      compare_reduced(+Target,+Sample,+Metric,+Limit) is det.
%
%       Compare a learned hypothesis with and without atomic residue.
%
%       Target is the predicate indicator of the learning target. Data
%       for the learning target is collected from the currently loaded
%       experiment file.
%
%       Sample should be a term of the form P/N, where each of P and N
%       is an integer, or a float, denoting the size of the training
%       partition for the positive (P) and negative (N) examples,
%       respectively.
%
%       Metric is one of the metrics recognised by predicates in the
%       module evaluation.pl.
%
%       Limit is an integer denoting the maximum number of clauses of
%       learned hypotheses and atomic residue to be printed to the
%       output.
%
%       This predicate learns a hypothesis for the given Target from
%       the MIL problem in the current experiment file with examples
%       partitioned into training and held-out test partitions
%       according to Sample. The hypothesis is reduced with the current
%       setting of reduction/1, which should normally be set to
%       "plotkins", otherwise it won't make a lot of sense (and you'll
%       get a nice error anyway to remind you).
%
%       Once the hypothesis is learned and reduced, it is split to
%       atomic residue, i.e. atoms (unit clauses) not entailed by the
%       rest of the hypothesis, and the rest of the hypothesis, or
%       "remainder". The entire hypothesis, remainder, and atomic
%       residue, are then evaluated on the held-out testing partition.
%
%       The number of clauses in each of the hypothesis, remainder and
%       atomic residue, are printe in the output, as is the accuracy, or
%       whatever other Metric is given, for each set of clauses.
%
%       The point of the exercise is to be able to be able to tell how
%       much of the accuracy of a hypothesis is due to its atomic
%       residue. Normally the answer should be: "none", since the atomic
%       residue is simply example atoms in the training partition that
%       were not removed by Plotkin's reduction, and the training and
%       test partitions are disjoint. This is checked in
%       compare_reduced/4 btw, just in case. Since the two partitions
%       are disjoint, the performance on the test partition does not
%       depend on some of the residual atoms matching testing examples.
%
%       It follows that if the performance by Metric of the full
%       hypothesis with and without the atomic residue differ, the
%       atomic residue are resolving with clause in the remainder
%       hypothesis to entail some of the testing examples.
%
%       This first became apparent in the data(robots) experiment, where
%       a clause would often be learned of the form:
%       ==
%       move(A,B):- move(A,C), move(C,B).
%       ==
%
%       This clause permits the atomic residue to act as a "bridge"
%       between the start and end state in examples in robots.pl, while
%       possibly also resolving with other, non-unit, clauses in the
%       remainder of the hypothesis.
%
%       The anti-recursive metarule constraint at the start of this file
%       can be used to remove recursive clauses like the one above from
%       the learned hypothesis and observe this "bridging" effect in
%       practice.
%
compare_reduced(T,S,M,L):-
        experiment_data(T,Pos,Neg,BK,MS)
        % Split examples to training and test partitions
        ,debug(compare_reduced,'Partitioning data...',[])
        ,evaluation:data_partitions(S,Pos,Neg,Pos_Train,Neg_Train,Pos_Test,Neg_Test)
        % Check that partitions are really disjoint.
        ,(   disjoint_partitions(Pos_Train,Pos_Test)
         ->  true
         ;   throw('Positive training and test partitions are not disjoint!')
         )
        ,(   disjoint_partitions(Neg_Train,Neg_Test)
         ->  true
         ;   throw('Negative training and test partitions are not disjoint!')
         )
        % Encapsulate training partition
        ,debug(compare_reduced,'Encapsulating problem...',[])
        ,encapsulated_problem(Pos_Train,Neg_Train,BK,MS,[Pos_Train_e,Neg_Train_e,BK_,MS_])
        ,(   configuration:reduction(plotkins)
         ->  true
         ;   throw('Set reduction to \'plotkins\' for this test.')
         )
        % Train on the encapsulated training partitions
        ,debug(compare_reduced,'Constructing Top Program...',[])
        ,learn(Pos_Train_e,Neg_Train_e,BK_,MS_,Ps)
        ,length(Ps,L1)
        ,debug(compare_reduced,'Constructed ~w clauses.',[L1])
        % Evaluate the learned hypothesis on the test partition
        ,debug(compare_reduced,'Evaluating Top Program...',[])
        ,evaluate(T,Ps,BK,Pos_Test,Neg_Test,M,V1)
        % Remove atomic residue from hypothesis
        ,atomic_residue(Ps,Rs,As)
        % Evaluate the remainder hypothesis on the test partition
        ,debug(compare_reduced,'Evaluating remainder of Top Program...',[])
        ,evaluate(T,Rs,BK,Pos_Test,Neg_Test,M,V2)

        % Evaluate the atomic residue on the test partition
        ,debug(compare_reduced,'Evaluating atomic residue of Top Program...',[])
        ,evaluate(T,As,BK,Pos_Test,Neg_Test,M,V3)

        % Check overlap between atomic residue and examples.
        ,whence_residue(As,Pos_Train,training,_Is_train)
        ,whence_residue(As,Pos_Test,testing,_Is_test)
        ,debug(compare_reduced,'Reporting...',[])
        ,report('Learned:',Ps,M,V1,L)
        ,report('Remainder:',Rs,M,V2,L)
        ,report('Residue:',As,M,V3,L).


%!      whence_residue(+Atoms,+Examples,+Partition,-Intersection) is det.
%
%       Test whether Atomic residue and Examples overlap.
%
%       Atoms are the atomic residue in a hypothesis.
%
%       Examples are the ground atoms in a set of examples.
%
%       Partition is an atom that says which partition Examples are
%       from: training, or testing.
%
%       Intersection is the intersection of the set of atoms in Atoms
%       and Examples.
%
whence_residue(As,Pos,P,Is):-
        debug(compare_reduced,'Looking for residual atoms in ~w set...',[P])
        ,maplist(sort,[As,Pos],[As_,Pos_])
        ,ord_intersection(Pos_,As_,Is)
        ,length(Is, N)
        ,debug(compare_reduced,'Found ~w residual atoms in ~w set.',[N,P]).


%!      atomic_residue(+Clauses,-Remainder,-Residue) is det.
%
%       Split atomic Residue out of the Clauses of a learned hypothesis.
%
%       Clauses are the clauses of a learned hypothesis.
%
%       Remainder are the non-unit clauses in Clauses.
%
%       Residue are the atomic, unit clauses in Clauses.
%
%       "Atomic residue" is my name for the atoms that are output
%       together with a learned hypothesis when reduction is set to
%       "plotkins", either because the other clauses in the hypothesis
%       do not entail those atoms, or because the reduction is not
%       strong enough to remove all of them.
%
atomic_residue(Cs,Rs,As):-
        debug(compare_reduced,'Handling residue...',[])
        ,atomic_residue(Cs,[],Rs,[],As)
        ,maplist(length,[Rs,As],[N,M])
        ,debug(compare_reduced,'Found ~w residue atoms, ~w other clauses.',[M,N]).

%!      atomic_residue(+Clauses,+Acc_Rem,-Rem,+Acc_Res,-Res) is det.
%
%       Business end of atomic_residuce/3.
%
atomic_residue([],Rs,Rs_r,As,As_r):-
        maplist(reverse,[Rs,As],[Rs_r,As_r])
        ,!.
atomic_residue([H:-B|Cs],Rs_Acc,Rs,As_Acc,As):-
        !
        ,atomic_residue(Cs,[H:-B|Rs_Acc],Rs,As_Acc,As).
atomic_residue([A|Cs],Rs_Acc,Rs,As_Acc,As):-
        atomic_residue(Cs,Rs_Acc,Rs,[A|As_Acc],As).


%!      disjoint_partitions(+Training,+Testing) is det.
%
%       True when Training intersction Testing = [].
%
disjoint_partitions(Training,Testing):-
        maplist(sort,[Training,Testing],[T1,T2])
        ,ord_intersection(T1,T2,[]).


%!      evaluatie(+Tgt,+Prog,+BK,+Pos_Tst,+Neg_Tst,+Metric,-Val) is det.
%
%       Evaluate a Program on a testing partition.
%
evaluate(T,Ps,BK,Pos_Test,Neg_Test,M,V):-
        evaluation:program_results(T,Ps,BK,Rs)
        ,evaluation:evaluation(Rs,Pos_Test,Neg_Test,_Ts,_Bs,Cs)
        ,once(evaluation:metric(M,Cs,V)).


%!      report(+What,+Prog,+Metric,+Value,+Limit) is det.
%
%       Print out a report on the performance of a Program.
%
%       Limit is an integer, or the atom 'inf': how many clauses of the
%       program to print out. 'inf' does _not_ print an infinite number
%       of clauses, only all the ones in Prog.
%
report(R,Ps,M,V,L):-
        writeln(R)
        ,auxiliaries:print_limited(L,Ps)
        ,length(Ps,N)
        ,format('Number of clauses: ~w~n',[N])
        ,format('~w: ~w: ~n',[M,V])
        ,nl.
