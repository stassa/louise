:-module(debugging, [debug_lines/3
                    ]).

/** <module> Debugging facilities for vision_thing dataset.

Example of use:

==
?- _N = 4, _Ts = [line,cross,square,rectangle,point], member(_T,_Ts), learn_dynamic(_T/_N,_Ps), reduce_unfolded(_Ps,_Rs), experiment_data(_T/_N,_Pos,_Neg,_BK,_MS), debug_lines(_Pos,_BK,_Rs).
Learned shape grammar:
line(A,B,C,D):-horizontal_line(A,B,C,D).
line(A,B,C,D):-vertical_line(A,B,C,D).

Object cells:
[cell(1,3-3,1/0),cell(1,3-3,1/1),cell(1,3-3,1/2)].
. b .
. b .
. b .

Parsed vertical line:
[cell(1,3-3,1/0),cell(1,3-3,1/1),cell(1,3-3,1/2)].
. b .
. b .
. b .

true .
==

*/

%!      debugging_module(?Module) is semidet.
%
%       Name of the module where debugging terms are asserted.
%
%       Used to assert learned hypotheses in order to separate them more
%       or less cleanly from the rest of the project. More or less.
%
debugging_module('$debugging').


%!      debug_lines(+Examples,+BK,+Grammar) is det.
%
%       Debug a segmentation Grammar showing how Examples are parsed.
%
%       Examples is a list of examples for a learning target. BK is the
%       set of predicate symbols and arities of background knowledge
%       predicates for that target. Grammar is a list of clauses in a
%       hypothesis, representing a line segmentation grammar for the
%       shapes in the given Examples.
%
%       debug_lines/3 calls the learned Grammar to parse each example
%       in Examples and prints out the resulting sub-objects parsed from
%       that example by Grammar.
%
debug_lines(Es,BK,Ps):-
        debugging_module(MD)
        ,experiment_file(_,ME)
        ,print_clauses('Learned shape grammar:', Ps)
        ,nl
        ,closure(BK,ME,Bs)
        ,flatten([Es,Ps,Bs],Ts)
        ,S = assert_program(MD,Ts,Rs)
        ,G = (member(E,Es)
             ,parse_shape(MD,E)
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
        ,debug_lines(E)
        ,!.
parse_shape(_M,_E):-
        debug_lines([]).


%!      debug_cells(+Example) is det.
%
%       Portray the cells of an object in an Example.
%
debug_cells(E):-
        example_cells(E,Cs)
        ,writeln('Object cells:')
        ,portray_object(Cs)
        ,nl.

%!      debug_lines(+Example) is det.
%
%       Portray the cells in a shape parsed from an Example.
%
debug_lines(E):-
        example_lines(E,Ls_)
        % Lines are reversed in the accumulator in examples.
        ,reverse(Ls_,Ls)
        ,forall(member(Ls_i,Ls)
               ,(phrase(shape(T,O,Cs),[Ls_i])
                ,format('Parsed ~w ~w:~n',[T,O])
                ,portray_object(Cs)
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


%!      shape(-Orientation,-Type,-Cells)// is nondet.
%
%       Determine a parsed object's shape.
%
shape(O,line,Cs) --> line(O,Cs).
shape(single,point,Cs) --> point(Cs).

%!      line(-Orientation,-Type)// is nondet.
%
%       Determine a parsed line's Orientation and Type.
%
line(vertical,Cs) --> [vl(Cs)].
line(horizontal,Cs) --> [hl(Cs)].

%!      point(-Cells)// is nondet.
%
%       The parsed object is a single point.
%
point(Cs) --> [pt(Cs)].
