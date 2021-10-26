:-module(thelma_cofiguration, [default_ordering/1
                              ,depth_limits/2
                              ,order_constraints/5
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


%!	order_constraints(+M,+Second_Order,+First_Order,+SO_Constraints,+FO_Constraints)
%!	is det.
%
%	A set of order constraints for a metarule, M.
%
%	Lists order constraints for commonly used metarules. Like
%	metarule/4, this predicate is also marked as multifile so
%	experiment files can declare their own special order
%	constraints. See notes in metarule/4.
%
order_constraints(unit,_Ss,_Fs,[],[]).
order_constraints(projection_21,[P,Q],_Fs,[P>Q],[]).
order_constraints(projection_12,[P,Q],_Fs,[P>Q],[]).
order_constraints(inverse,[P,Q],_Fs,[P>Q],[]).
order_constraints(identity,[P,Q],_Fs,[P>Q],[]).
order_constraints(chain,[P,Q,R],_Fs,[P>Q,P>R],[]).
% One less constraint - for anbn.pl
%order_constraints(chain,[P,Q,_R],_Fs,[P>Q],[]).
order_constraints(tailrec,[P,Q],[X,Y,Z],[P>Q],[X>Z,Z>Y]).
order_constraints(precon,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(postcon,[P,Q,R],_Fs,[P>Q,P>R],[]).
order_constraints(switch,[P,Q,R],_Fs,[P>Q,P>R],[]).
