:-module(debugging, [debug_learned/3
                    ]).

/** <module> Debugging facilities for vision_thing dataset.

*/

%!      debugging_module(?Module) is semidet.
%
%       Name of the module where debugging terms are asserted.
%
%       Used to assert learned hypotheses in order to separate them more
%       or less cleanly from the rest of the project. More or less.
%
debugging_module('$debugging').

%!      experiment_file_module(?Module) is semidet.
%
%       Name of the experiment file Module.
%
%       Used to collect background predicates' definitions.
%
experiment_file_module(vision_thing).


%!      debug_learned(+Examples,+BK,+Program) is det.
%
%       Debug a learned Program showing how Examples are parsed.
%
%       Examples is a list of examples for a learning target. BK is the
%       set of predicate symbols and arities of background knowledge
%       predicates for that target. Program is a list of clauses in a
%       hypothesis learned from the given Examples.
%
%       debug_learned/3 calls the learned Program to parse each example
%       in Examples and prints out the resulting sub-objects parsed from
%       that example by Program.
%
debug_learned(Es,BK,Ps):-
        debugging_module(MD)
        ,experiment_file_module(ME)
        ,print_clauses('Learned shape grammar:', Ps)
        ,nl
        ,closure(BK,ME,Bs)
        ,flatten([Es,Ps,Bs],Ts)
        ,S = assert_program(MD,Ts,Rs)
        ,G = forall(member(E,Es)
                   ,(parse_shape(MD,E)
                    )
                   )
        ,C = erase_program_clauses(Rs)
        ,setup_call_cleanup(S,G,C).


%!      parse_shape(+Module,+Example) is nondet.
%
%       Parse a shape in an Example.
%
parse_shape(M,E):-
        debug_cells(E)
        ,clause(M:E,B)
        ,B \= true
        ,call(M:B)
        ,debug_lines(E).
parse_shape(_M,_E):-
        debug_lines([]).


%!      debug_cells(+Example) is det.
%
%       Portray the cells of an object in an Example.
%
debug_cells(E):-
        example_cells(E,Cs)
        ,writeln('Example cells:')
        ,portray_object(Cs)
        ,nl.

%!      debug_lines(+Example) is det.
%
%       Portray the cells in a shape parsed from an Example.
%
debug_lines(E):-
        example_lines(E,Ls)
        ,forall(member(Ls_i,Ls)
               ,(writeln('Parsed lines:')
                ,portray_object(Ls_i)
                ,nl
                )
               )
        ,!.
debug_lines([]):-
        writeln('Failed to parse shape!')
        ,nl.


%!      example_cells(+Example,-Cells) is det.
%
%       Extract a list of object Cells from an Example.
%
example_cells(E,Cs):-
        E =.. [_S,[],_Ls,[Cs],[[]]]
        ,!.
example_cells(E,Cs):-
        E =.. [_S,[Cs],[[]]]
        ,!.

%!      example_lines(+Example,-Lines) is det.
%
%       Extract a list of cells of a parsed shape from an Example.
%
example_lines(E,Ls):-
        %E =.. [_S,[],Ls,[_Cs],[[]]]
        E =.. [_S,[],Ls,[_Cs],_]
        ,!.
example_lines(E,[Cs]):-
        E =.. [_S,[Cs],_].
        %E =.. [_S,[Cs],[[]]].
