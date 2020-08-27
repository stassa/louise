:-module(lifting_configuration, [lift_compounds/1
                                ,lift_lists/1
                                ]).

/** <module> Configuration options for lifting.pl.

*/


%!      lift_compounds(?How) is semidet.
%
%       How to lift compound Prolog terms.
%
%       How is one of: [once, recursively].
%
%       If How is "once", each compound term (other than a list) found
%       as an argument of a literal is replaced by a single variable.
%
%       If How is "recursively", each compound term (other than a list)
%       found as an argument of a literal is variabilised recursively,
%       that is each of its sub-terms is bound to a new variable (or an
%       existing one if a matching term has already been lifted
%       earlier).
%
%       Note that if a compound has sub-terms that are lists, the value
%       of lift_lists/1 will determine how those sub-terms are lifted.
%
lift_compounds(once).
%lift_compounds(recursively).


%!      lift_list_elements(?How) is semidet.
%
%       How to lift lists.
%
%       How is one of: [once, recursively].
%
%       If How is once, each list found as an argument of a literal is
%       replaced by a single variable.
%
%       If How is recursively, each element of a list found as an
%       argument of a literal is lifted recursively, i.e. each of its
%       sub-terms is bound to a new variable (or an existing one if a
%       matching term has already been lifted).
%
%       Note that if a list has compound elements, the value of
%       lift_compounds/1 will determine how those elements are lifted.
%
lift_lists(once).
%lift_lists(recursively).
