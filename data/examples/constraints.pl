:-module(constraints, [background_knowledge/2
		      ,metarules/2
		      ,positive_example/2
		      ,negative_example/2
		      ,path/2
		      ,edge/2
		      ]).

/** <module> Experiment file demonstrating metarule constraints.

__1. Introduction__

This example defines two learning targets, right_rec/2 and left_rec/2.
Both are equivalent to path/2, listed below:

==
path(X,Y):-
	edge(X,Y).
path(X,Y):-
	edge(X,Z)
	,path(Z,Y).
==

A metarule constraint is declared that excludes from the Top program for
right_rec/2 left-recursive clauses:

==
configuration:metarule_constraints(M,fail):-
	M =.. [m,_Id,right_rec,right_rec|_Ps].
==

This constraint will match generalising metasubstitutions with any
metarule Id and where the first two existentially quantified variables
are instantiated to right_rec, the predicate symbol of the right_rec/2
learning target. Such metasubstitutions, once applied to their
corresponding metarules, would cause left-recursive clauses to be added
to the Top program and the learned hypothesis. This is demonstrated in
the following sections.


__2. Known good configuration__

Ideal configuration options for this experiment file are as follows.
Important options are highlighted with an asterisk (*):

==
?- list_config.
* clause_limit(1)
depth_limits(2,1)
example_clauses(call)
* experiment_file(data/examples/constraints.pl,constraints)
fold_recursive(false)
generalise_learned_metarules(false)
learner(louise)
* max_invented(0)
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


__2. Learning query__

List learning results for the two learning targets with
list_learning_results/0:

==
?- list_learning_results.
right_rec(A,B):-edge(A,B).
right_rec(A,B):-edge(A,C),right_rec(C,B).

left_rec(A,B):-edge(A,B).
left_rec(A,B):-left_rec(A,C),left_rec(C,B).

true.
==

__ 3. Declaring constraints__

Metarule constraints are declared as clauses of the predicate
metarule_constraints/2:

==
configuration:metarule_constraints(+Metasubstitution,+Goal).
==

This predicate is declared as multifile in the configuration module. For
clauses of metarule_constraints/2 to be found during learning, they must
be prefixed with the module qualifier _configuration_.

During the "generalisse" step of Top program construction, when a
metasubstitution is found it is passed to the first argument of each
metarule_constraints/2 clause in the database and this clause is called.
If this first call succeeds, the second argument of the
metarule_constraints/2 clause is passed to call/1. If this second call
succeeds, the metasubstitution is included in the Top program;
otherwise, it is excluded from the Top program.

Metarule constraints can be used to exert fine control on the structure
of the Top program. Clauses of metarule_constraints/2 can be unit or
non-unit clauses and their second argument can be any Prolog goal. In
this way, by combining the two arguments of metarule_constraints/2,
constraints of arbitrary complexity can be implemented.

That said the main motivation for metarule constraints is to exclude
left-recursive clauses from the Top program, as shown in the examples in
this experiment file. Left-recursive clauses pose problems when executed
in Prolog but have an unfortunate tendency to crop up often in the
hypotheses learned by Louise when the target theory is recursive.
Excluding them by use of constraints is the simplest way to resolve such
problems.

For problems where the Top program does not include left-recursive
clauses the additional complexity of using constraints can be avoided
and one can instead rely on the examples, background knowledge and
metarules to control what is learend.

Of course, sometimes left-recursive clauses are desired, or not a
problem (for example when the learned hypothesis is meant to be
interpreted in a bottom-up fashion, or using SLG resolution etc) in
which case, again, constraints are not necessary. However, this is left
up to the user to decide hence anti-left recursion constraints are not
hard-coded in Top program construction.

__4. Using constraints__

Note that, in the listing in the previous section, the second
clause of the learned hypothesis for left_rec/2 is left-recursive
whereas the second clause in the hypothesis learned for right_rec/2 is
recursive, but not left-recursive. This is despite the fact that the MIL
problems for the two learning targets (i.e. the clauses of
background_knowledge/2, metarules/2, positive_example/2 and
negative_example/2) are identical up to the symbols of their target
predicates.

The reason the two hypotheses are different is because of the metarule
constraint declared for right_rec/2:

==
configuration:metarule_constraints(M,fail):-
	M =.. [m,_Id,right_rec,right_rec|_Ps].
==

This constraint will match any metasubstitution where the symbol of the
target predicate is right_rec and where each existentially quantified
variable is ground to right_rec. If the match succeeds, fail/0 will be
passd to call/1 and the entire constraint will fail, causing the
metasubstitution to be excluded from the Top program.

If we were to remove this constraint, left-recursive clauses of
right_rec/2 would be included in the Top program and so both hypotheses
would be identical up to the predicate names of their learning targets:

==
?- list_learning_results.
right_rec(A,B):-edge(A,B).
right_rec(A,B):-right_rec(A,C),right_rec(C,B).

left_rec(A,B):-edge(A,B).
left_rec(A,B):-left_rec(A,C),left_rec(C,B).

true.
==

The reason the left-recursive clause of right_rec/2 above would be
included in the learned hypothesis once the constraint in [1] is
removed, is that this clause is the most general clause of its
arity in the Top program for right_rec/2. This clause is most-general in
the sense that it entails all other clauses of the same arity in the
Top program for right_rec/2. That causes all other clauses of the same
arity to be discarded as redundant during Top program reduction (given
resolution(plotkins) is set in the configuration).

The Top program for right_rec/2 can be inspected using
list_top_program/2. With the second argument set to _true_ to apply the
metasubstitutions to their corresponding metarules and with the
constraint in [1] _removed_, the Top program for right_rec/2 is as
follows:

==
% "true" applies the measubstitutions in the Top program to their
% metarules, producing definite clauses.
?- list_top_program(right_rec/2, true).
Generalisation:
---------------
m(right_rec,A,B):-m(edge,A,B).
m(right_rec,A,B):-m(right_rec,A,B).
m(right_rec,A,B):-m(edge,A,C),m(right_rec,C,B).
m(right_rec,A,B):-m(right_rec,A,C),m(right_rec,C,B).
Length:4

Specialisation:
---------------
m(right_rec,A,B):-m(edge,A,B).
m(right_rec,A,B):-m(right_rec,A,B).
m(right_rec,A,B):-m(edge,A,C),m(right_rec,C,B).
m(right_rec,A,B):-m(right_rec,A,C),m(right_rec,C,B).
Length:4
true.
==

The left-recursive clause
m(right_rec,A,B):-m(right_rec,A,C),m(right_rec,C,B) subsumes the
right-recursive clause m(right_rec,A,B):-m(edge,A,C),m(right_rec,C,B).
As a result, in the Top program reduction step the right-recursive
clause is removed from the Top program as redundant by Plotkin's program
reduction algorithm.

Conversely, leaving the constraint in [1] in the database (i.e. not
removed as in the above example) the Top program for right_rec/2 is as
follows:

==
?- list_top_program(right_rec/2, true).
Generalisation:
---------------
m(right_rec,A,B):-m(edge,A,B).
m(right_rec,A,B):-m(edge,A,C),m(right_rec,C,B).
Length:2

Specialisation:
---------------
m(right_rec,A,B):-m(edge,A,B).
m(right_rec,A,B):-m(edge,A,C),m(right_rec,C,B).
Length:2
true.
==

As can be seen, the left-recursive clause in the previous listing is
removed already, before reduction.

The above observation highlights another effect of constraining the Top
program: by reducing the number of clauses passed to the reduction
algorithm it can reduce the cost of the reduction step of Louise's
learning procedure.

__5. Alternative constraints__

Alternatively to the constraint in [1], we could replace the right_rec
predicate symbol in the metarule constraint with a variable:

==
configuration:metarule_constraints(M,fail):-
	M =.. [m,_Id,P,P|_Ps].
==

In that case the constraint would also match left_rec/2
metasubstitutions and left-recursive clauses would be absent from the
learned hypothesis for left_rec/2 also:

==
?- list_learning_results.
right_rec(A,B):-edge(A,B).
right_rec(A,B):-edge(A,C),right_rec(C,B).

left_rec(A,B):-edge(A,B).
left_rec(A,B):-edge(A,C),left_rec(C,B).

true.
==

Similar results can be achieved with simpler constraints. For instance,
the following constraint will exclude left-recursive clauses from both
hypotheses:

==
configuration:metarule_constraints(m(_Id,P,P),fail). [3]
==

While the following constraint will only exclude left-recursive clauses
from the hypothesis for right_rec/2:

==
configuration:metarule_constraints(m(tailrec,right_rec,right_rec),fail). [4]
==

The difference is that the two simpler constraints in [3,4] above will
only match metasubstitutions with exactly 3 existentially quantified
variables, whereas the two more complex constraints in [1,2] will match
metasubstitutions with any number of existentially quantified variables.


*/

% Constraint excluding left-recursive clauses of right_rec/2 from the
% Top program.
configuration:metarule_constraints(M,fail):-
	M =.. [m,_Id,right_rec,right_rec|_Ps].

/* Also	try these alterantive constraints:

% Constraint excluding left-recursive clauses of any target predicate
% from the Top program.
%
%configuration:metarule_constraints(M,fail):-
%	M =.. [m,_Id,P,P|_Ps].

% Constraint excluding left-recursive clauses that are instances of a
% metarule with any Id and having two existentially quantified
% variables. This matches both Tailrec and Identity
%
%configuration:metarule_constraints(m(_Id,P,P),fail).

% Constraint excluding left-recursive clauses of the Tailrec and
% Identity metarules where the target predicate symbol is right_rec.
%configuration:metarule_constraints(m(tailrec,right_rec,right_rec),fail).

*/

background_knowledge(right_rec/2,[edge/2]).
background_knowledge(left_rec/2,[edge/2]).

metarules(right_rec/2,[tailrec,identity]).
metarules(left_rec/2,[tailrec,identity]).

positive_example(right_rec/2,right_rec(A,B)):-
	path(A,B).
positive_example(left_rec/2,left_rec(A,B)):-
	path(A,B).

negative_example(right_rec/2,right_rec(A,B)):-
	path(B,A).
negative_example(left_rec/2,left_rec(A,B)):-
	path(B,A).

path(X,Y):-
	edge(X,Y).
path(X,Y):-
	edge(X,Z)
	,path(Z,Y).

edge(a, b).
edge(c, d).
edge(b, e).

edge(f, b).
edge(g, d).
edge(d, e).
