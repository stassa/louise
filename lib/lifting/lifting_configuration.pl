:-module(lifting_configuration, [lift_compounds/1
                                ,lift_lists/1
                                ,lift_skolemised/1
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


%!      lift_skolemised(?Bool) is semidet.
%
%       Whether to lift skolemised terms '$VAR'(I) etc.
%
%       Options are:
%       * true: treat skolemised terms as compounds as described by
%       lift_compounds/1.
%       * false: do not lift skolemised terms.
%
%       Skolemised terms are atoms of the predicate '$VAR'/1 that are
%       normally used to number variables, like '$VAR'(0), '$VAR'(1)
%       etc, as used in the lifting module itself. However, Swi-Prolog
%       accepts '$VAR'/1 atoms with a constant that is not a number, for
%       examle '$VAR'('X'), '$VAR'('Y') etc. These are used to give
%       specific names to variables. This option determines what happens
%       when such terms are found in literals.
%
%       Some examples will help elucidate the relation between this
%       option and the option lift_compounds/1.
%
%       If lift_skolemised/1 is set to "false", skolemised terms are
%       left untouched:
%       ==
%       ?- _T = p('$VAR'('X'))
%       ,lifted_program([_T], _Ls)
%       ,write_canonical(_Ls)
%       ,lift_compounds(Lift_compounds)
%       ,lift_skolemised(Lift_skolemised).
%       [p('$VAR'('X'))]
%       Lift_compounds = once,
%       Lift_skolemised = false.
%
%
%       ?- _T = p('$VAR'('X'))
%       ,lifted_program([_T], _Ls)
%       ,write_canonical(_Ls)
%       ,lift_compounds(Lift_compounds)
%       ,lift_skolemised(Lift_skolemised).
%       [p('$VAR'('X'))]
%       Lift_compounds = recursively,
%       Lift_skolemised = false.
%       ==
%
%       Otherwise skolemised terms are lifted as any compound term:
%       ==
%       ?- _T = p('$VAR'('X'))
%       ,lifted_program([_T], _Ls)
%       ,write_canonical(_Ls)
%       ,lift_compounds(Lift_compounds)
%       ,lift_skolemised(Lift_skolemised).
%       [p(_)]
%       Lift_compounds = once,
%       Lift_skolemised = true.
%
%
%       ?- _T = p('$VAR'('X'))
%       ,lifted_program([_T], _Ls)
%       ,write_canonical(_Ls)
%       ,lift_compounds(Lift_compounds)
%       ,lift_skolemised(Lift_skolemised).
%       [p('$VAR'(_))]
%       Lift_compounds = recursively,
%       Lift_skolemised = true.
%       ==
%
%       @tbd Note that strickly speaking you can use any constant in
%       place of 'true' in lift_skolemised/1. The implementation only
%       recognises "false" and avoids lifting a skolemised term. Not
%       very good practice that so don't do it just because I did. And
%       don't tell anyone I did it.
%
%lift_skolemised(true).
lift_skolemised(false).
