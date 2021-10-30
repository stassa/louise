:-module(thelma_cofiguration, [default_ordering/1
                              ,depth_limits/2
                              ,order_constraints/5
                              ,metarule_functor/1
                              ]).

/** <module> Configuration options for Thelma.

*/


%!	thelma_default_ordering(?Order) is semidet.
%
%	The default for automatically assigned interval ordering.
%
%	Order is one of [higher,lower] and determines what to do when a
%	constant is assigned multiple indexings in the process of
%	automatically determining its interval ordering.
%
default_ordering(lower).


%!      thelma_depth_limits(?Clauses,?Invented) is semidet.
%
%       Depth limits for Thelma's hypothesis search.
%
%       Clauses is the maximum number of clauses in hypotheses
%       considered in the Hypothesis Space search restricted by these
%       depth limits. Invented is the maximum number of clauses of
%       invented predicates in hypotheses.
%
%       If Invented is not at most 1 lower than Clauses, it will be
%       treated as being one lower than Clauses.
%
depth_limits(2,1).


%!      order_constraints(+Id,+Existential,+FO,+Lexicographic,+Interval)
%       is semidet.
%
%	Metarule order constraints for Thelma.
%
%       Id is an atom, the id of a metarule for which these constraints
%       are defined.
%
%       Existential is the list of existentially quantified variables in
%       the identified metarule. This list must include both
%       second-order and first-order existentially quantified variables.
%
%       FO is the list of first-order variables in the identified
%       metarule. This list must include both universally and
%       existentially quantified first-order variables.
%
%       Lexicographic is a list of lexicographic order constraints over
%       the second-order varibles in Existential. Lexicographic order
%       constraints are defined as pairs P>Q where P, Q are second-order
%       variables included in the list Existentials. The meaning of P>Q
%       is that any predicate symbol bound to P during learning is above
%       any predicate symbol bound to Q, in the lexicographic order of
%       predicate symbols.
%
%       Interval is a list of interval order constraints over the
%       first-order variables in a metarule. Interval order constraints
%       are defined as pairs X>Y, with the same meaning as for
%       lexicographic order constraints, except here X and Y are
%       first-order variables that bind to constants, rather than
%       predicate symbols, during learning.
%
%       __Motivation__
%
%       Order constraints are used in the originally described version
%       of Metagol to ensure termination when learning recursion. They
%       impose a total ordering over the Herbrand base used to restrict
%       the instantiations of variables in metarules.
%
%       Lexicographic order constraints ensure termination of the
%       learning procedure when learning recursive hypotheses. Interval
%       order constraints are necessary when hypotheses include
%       mutually recursive predicates, most notably when an auxiliary
%       predicate that is mutually recursive with the target predicate
%       must be invented in order to learn the target.
%
%       Lexicographic and interval order constraints are more fully
%       explained in the following reference:
%       ==
%       Meta-interpretive learning of higher-order dyadic datalog:
%       predicate invention revisited
%       Mach Learning (2015) 100:49-73
%       Muggleton, et al.
%       ==
%
%       @tbd Order constraints require a total ordering over the
%       predicate symbols and constants in a learning problem. Such an
%       ordering is derived automatically from the order in which
%       background predicates' definitions and examples are declared in
%       an experiment file. This is achieved by a call to
%       order_constraints/3. See that predicate for more information,
%       including the ability to manually defined a total ordering.
%       Ouch.
%
%       @bug Currently, only lexicographic order constraints are
%       implemented in Thelma. Defining interval order constraints won't
%       have any effect at all unless this is resolved.
%
order_constraints(unit,_Ss,_Fs,[],[]).
order_constraints(projection_21,[P,Q],_Fs,[P>Q],[]).
order_constraints(projection_12,[P,Q],_Fs,[P>Q],[]).
order_constraints(inverse,[P,Q],_Fs,[P>Q],[]).
order_constraints(identity,[P,Q],_Fs,[P>Q],[]).
%order_constraints(chain,[P,Q,R],_Fs,[P>Q,P>R],[]).
% One less constraint - for anbn.pl
order_constraints(chain,[P,Q,_R],_Fs,[P>Q],[]).
order_constraints(tailrec,[P,Q],[X,Y,Z],[P>Q],[X>Z,Z>Y]).
order_constraints(precon,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(postcon,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(switch,[P,Q,R],_Fs,[P>Q,P>R],[]).

% data/examples/yamamoto.pl
order_constraints(postcon_unit,[P,Q,_R],_Fs,[P>Q],[]).

% data/drafts/one_shot/nat.pl
%order_constraints(m1,[P,Q],_Fs,[P>Q],[]).
%order_constraints(m2,[P,Q,_R],_Fs,[P>Q],[]).

% data/drafts/one_shot/even_odd.pl
order_constraints(m1,[P,Q],_Fs,[P>Q],[]).
order_constraints(m2,[P,Q,_R],_Fs,[P>Q],[]).

% data/drafts/one_shot/findlast.pl
%order_constraints(postcon1,[P,Q,_R],_Fs,[P>Q],[]).
%order_constraints(precon1,[P,Q,_R],_Fs,[P>Q],[]).

% data/drafts/one_shot/findlast_acyclic.pl
%order_constraints(tailrec1,[P,Q],_Fs,[P>Q],[]).
%order_constraints(midcon,[P,Q,_R,_S],_Fs,[P>Q],[]).

% Catchall constraint.
%order_constraints(_,_Ss,_Fs,[],[]).


%!	metarule_functor(?Functor) is semidet.
%
%	Functor for the internal representation of metarules.
%
metarule_functor('$metarule').
