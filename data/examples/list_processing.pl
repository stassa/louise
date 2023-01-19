:-module(list_proc, [background_knowledge/2
                    ,metarules/2
                    ,positive_example/2
                    ,negative_example/2
                    ,zero/1
                    ,empty/1
                    ,head/2
                    ,tail/2
                    ,p/2
                    ]).

:-use_module(configuration).

/** <module> Example of learning from non-datalog examples.

The learning targets in this experiment file are common list-processing
predicates found in all Prolog libraries: length/2, member/2, append/3
and reverse/3.

Learning such programs with Louise requires "flattening" to remove
functional terms from the heads of background knowledge (BK) predicates.
Flattening is necessary because metarules are restricted to datalog,
i.e. definite clauses with no function symbols other than constants. In
list-processing predicates the flattening is mostly needed to learn
clauses with accumulator variables.

In list-processing programs accumulators are typically bound to
list-constructors, of the form [H|T]. These are functional terms and so
cannot be in metarules. For example, it is not allowed to define a
metarule like this:

==
(Mem-Two) ∃.P,Q ∀.x,h,t: P(x,[h|t])← Q(x,t)
==

Even though this metarule would directly match the second, recursive
clause of the classic definition of member/2 in Prolog:

==
member(X, [X|T]).  
member(X, [H|T]):- member(X,T).  
==

Instead, we must give metarules with additional literals for flattening
predicates, and definitions of those predicates in the background
knowledge. For member/2, we give the following:

==
% Metarules
(Inverse) ∃.P,Q ∀.x,y: P(x,y)← Q(y,x)
(List-rec) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(y,z),R(x,z)

% BK
head([A|B],A):-A\=[C|D].
tail([A|B],B):-A\=[C|D].
==

That way, head/2 and tail/2 are used in the place of [H|T]. Then, the
additional literals in the two metarules can be instantiated with head/2
and tail/2 to give the flattened version of member/2, below:

==
member(A,B):-head(B,A).
member(A,B):-tail(B,C),membe_(A,C).
==

The motivation for keeping metarules restricted to datalog is that
datalog is decidable, whereas full definite logic is only semi-decidable
(and who knows what Prolog is- it depends on the implementation). This
comes at the cost of higher complexity when defining, or learning,
metarules for problems where decidability is not an issue and we just
want to learn a Prolog program.


Known good configuration
------------------------

To reproduce the results in this experiment file, make sure your
configuration options match the ones listed below. Important options are
marked with "*":

==
?- list_config.
* clause_limit(2)
example_clauses(call)
* experiment_file(data/examples/list_processing.pl,list_processing)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
listing_limit(10)
* max_error(0,0)
vmax_invented(0)
metarule_formatting(quantified)
metarule_learning_limits(none)
minimal_program_size(2,inf)
recursive_reduction(false)
reduce_learned_metarules(false)
* reduction(plotkins)
* resolutions(5000)
theorem_prover(resolution)
unfold_invented(false)
true.
==


Learning problems
-----------------

Make sure the elements of the MIL Problems for the four learning
targets match the ones listed below.

Note that the symbols of the learning targets are length_, member_,
append_ and reverse_, with an underscore, "_" to avoid clashing with
built-ins.

==
% length/2:
?- list_mil_problem(length_/2).
Positive examples
-----------------
length_([1,2,3,4,5,6,7],7).

Negative examples
-----------------

Background knowledge
--------------------
empty/1:
empty([]).

zero/1:
zero(0).

tail/2:
tail([A|B],B):-A\=[C|D].

p/2:
p(A,B):-number(A),var(B),!,succ(B,A).
p(A,B):-var(A),number(B),succ(B,A).

Metarules
---------
(List-id) ∃.P,Q,R ∀.x,y: P(x,y)← Q(x),R(y)
(List-rec-func) ∃.P,Q,R,S ∀.x,y,z,u: P(x,y)← Q(x,z),R(y,u),S(z,u)

true.


% member/2:
?- list_mil_problem(member_/2).
Positive examples
-----------------
member_(4,[1,2,3,4,5]).

Negative examples
-----------------

Background knowledge
--------------------
head/2:
head([A|B],A):-A\=[C|D].

tail/2:
tail([A|B],B):-A\=[C|D].

Metarules
---------
(Inverse) ∃.P,Q ∀.x,y: P(x,y)← Q(y,x)
(List-rec) ∃.P,Q,R ∀.x,y,z: P(x,y)← Q(y,z),R(x,z)

true.


% append/3:
?- list_mil_problem(append_/3).
Positive examples
-----------------
append_([a,b,c],[d,e,f],[a,b,c,d,e,f]).

Negative examples
-----------------

Background knowledge
--------------------
empty/1:
empty([]).

head/2:
head([A|B],A):-A\=[C|D].

tail/2:
tail([A|B],B):-A\=[C|D].

Metarules
---------
(List-id-triadic) ∃.P,Q ∀.x,y: P(x,y,y)← Q(x)
(List-acc-head) ∃.P,Q,R,S,T,P1 ∀.x,y,z,u,v,w: P(x,y,z)← Q(x,u),R(x,v),S(z,u),T(z,w),P1(v,y,w)

true.


% reverse/3:
?- list_mil_problem(reverse_/3).
Positive examples
-----------------
reverse_([a,b,c],[],[c,b,a]).

Negative examples
-----------------

Background knowledge
--------------------
empty/1:
empty([]).

head/2:
head([A|B],A):-A\=[C|D].

tail/2:
tail([A|B],B):-A\=[C|D].

Metarules
---------
(List-id-triadic) ∃.P,Q ∀.x,y: P(x,y,y)← Q(x)
(List-acc-body) ∃.P,Q,R,S,T,P1 ∀.x,y,z,u,v,w: P(x,y,z)← Q(x,u),R(x,v),S(w,u),T(w,y),P1(v,w,z)

true.
==


Learning query
--------------

You can call list_learning_results/0 to learn all four targets at once:

==
?- list_learning_results.
length_(A,B):-empty(A),zero(B).
length_(A,B):-tail(A,C),p(B,D),length_(C,D).

member_(A,B):-head(B,A).
member_(A,B):-tail(B,C),member_(A,C).

append_(A,B,B):-empty(A).
append_(A,B,C):-head(A,D),tail(A,E),head(C,D),tail(C,F),append_(E,B,F).

reverse_(A,B,B):-empty(A).
reverse_(A,B,C):-head(A,D),tail(A,E),head(F,D),tail(F,B),reverse_(E,F,C).

true.
==

*/


:- auxiliaries:set_configuration_option(clause_limit, [2]).

configuration:list_id metarule 'P(x,y):- Q(x) ,R(y)'.
configuration:list_id_triadic metarule 'P(x,y,y):- Q(x)'.
configuration:list_rec metarule 'P(x,y):- Q(y,z),R(x,z)'.
configuration:list_rec_func metarule 'P(x,y):- Q(x,z),R(y,u),S(z,u)'.
configuration:list_acc_head metarule 'P(x,y,z):-Q(x,u),R(x,v),S(z,u),T(z,w),F(v,y,w)'.
configuration:list_acc_body metarule 'P(x,y,z):-Q(x,u),R(x,v),S(w,u),T(w,y),F(v,w,z)'.

background_knowledge(length_/2, [empty/1,zero/1,tail/2,p/2]).
background_knowledge(member_/2, [head/2,tail/2]).
background_knowledge(append_/3, [empty/1,head/2,tail/2]).
background_knowledge(reverse_/3, [empty/1,head/2,tail/2]).

metarules(length_/2,[list_id,list_rec_func]).
metarules(member_/2,[inverse,list_rec]).
metarules(append_/3,[list_id_triadic,list_acc_head]).
metarules(reverse_/3,[list_id_triadic,list_acc_body]).

positive_example(length_/2,E):-
        member(E,[length_([1,2,3,4,5,6,7],7)
                 ]).

positive_example(member_/2,E):-
        member(E,[member_(4,[1,2,3,4,5])
               ]).

positive_example(append_/3,E):-
        member(E, [append_([a,b,c],[d,e,f],[a,b,c,d,e,f])
                  ]).

positive_example(reverse_/3,E):-
        member(E, [reverse_([a,b,c],[],[c,b,a])
                  ]).

negative_example(_,_):-
        fail.


%!      empty(?E) is semidet.
%
%       The empty list.
%
empty([]).


%!      head(?List,?Head) is semidet.
%
%       True when Head is the head of List.
%
head([H|_T],H):-
        H \= [_|_].


%!      tail(?List,?Tail) is semidet.
%
%       True when Tail is the tail of List.
%
tail([H|T],T):-
        H \= [_|_].


%!      zero(?Zero) is semidet.
%
%       i.e. nothing.
%
zero(0).


%!      p(?X,?Y) is det.
%
%       True when X is the predecessor of Y.
%
p(X,Y):-
        number(X)
        ,var(Y)
        ,!
        ,succ(Y,X).
p(X,Y):-
        var(X)
        ,number(Y)
        ,succ(Y,X).
