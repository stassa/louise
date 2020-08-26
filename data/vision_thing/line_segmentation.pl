:-module(line_segmentation, [write_segmentation_grammar/1
			    ,background_knowledge/2
                            ,metarules/2
                            ,positive_example/2
                            ,negative_example/2
                             % Background Knowledge productions.
                            ,vertical_line//2
                            ,horizontal_line//2
                            ,single_point//2
                             % Elementary shapes
                            ,vertical_line/3
                            ,horizontal_line/3
                            ,single_point/3
                            ]).

:-use_module(vision_thing).
:-use_module(vision_thing_config).
:-use_module(configuration).

/** <module> Parse objects in an ARC dataset image to line segments.

This experiment file defines a set of MIL problems where the learning
targets are line segmentation grammars for objects in raw images in the
ARC dataset format.

What is a "Line segmentation grammar"?
--------------------------------------

A "line segmentation grammar" is a grammar that splits an object into a
set of contiguous lines.

The following is an example of grammars learned by Louise from the MIL
problems defined in this experiment file:

==
?- _N = 4, _Ts = [line,cross,square,rectangle,point], member(_T,_Ts), learn_dynamic(_T/_N,_Ps), reduce_unfolded(_Ps,_Rs), print_clauses(_Rs).
line(A,B,C,D):-horizontal_line(A,B,C,D).
line(A,B,C,D):-vertical_line(A,B,C,D).
true ;
cross(A,B,C,D):-vertical_line(A,E,C,F),horizontal_line(E,G,F,H),horizontal_line(G,B,H,D).
true ;
square(A,B,C,D):-horizontal_line(A,E,C,F),horizontal_line(E,B,F,D).
square(A,B,C,D):-vertical_line(A,E,C,F),vertical_line(E,B,F,D).
true ;
rectangle(A,B,C,D):-horizontal_line(A,E,C,F),horizontal_line(E,B,F,D).
rectangle(A,B,C,D):-vertical_line(A,E,C,F),rectangle(E,B,F,D).
true ;
point(A,B,C,D):-single_point(A,B,C,D).
true.
==

In the example above, each target predicate (line/4, cross/4, etc)
represents a particular kind of shape, obtained by parsing an object in
a raw image in the ARC image format, from image_data.pl.

Each learned hypothesis is a Context-Free Grammar in Definite Clause
Grammar form, split into sets of continguous lines (or points) along
with their orientation (horizontal or vertical) in the 2-dimensional
plane of an image.

Note that the learned grammars only represent the composition of an
object as a set of lines and the orientation of the component lines, but
they do tell us nothing about the way the component lines are
articulated, i.e. how we put the component lines together to compose the
final object.

The former task, shape drawing, is the purpose of learning shape drawing
plans as defined in shape_drawing.pl, where the grammars learned from
the MIL problems in this experiment file are taken as background
knowledge.

The following example shows the segmentation of a cross object into
three lines, one vertical and two horizontal lines:

==
?- learn_dynamic(cross/4,_Ps), reduce_unfolded(_Ps,_Rs), experiment_data(cross/4,_Pos,_Neg,_BK,_MS), debug_learned(_Pos,_BK,_Rs).
Learned shape grammar:
cross(A,B,C,D):-vertical_line(A,E,C,F),horizontal_line(E,G,F,H),horizontal_line(G,B,H,D).

Object cells:
[cell(1,7-5,3/0),cell(1,7-5,3/1),cell(1,7-5,1/2),cell(1,7-5,2/2),cell(1,7-5,3/2),cell(1,7-5,4/2),cell(1,7-5,5/2),cell(1,7-5,3/3),cell(1,7-5,3/4)].
. . . b . . .
. . . b . . .
. b b b b b .
. . . b . . .
. . . b . . .

Parsed vertical line:
[cell(1,7-5,3/0),cell(1,7-5,3/1),cell(1,7-5,3/2),cell(1,7-5,3/3),cell(1,7-5,3/4)].
. . . b . . .
. . . b . . .
. . . b . . .
. . . b . . .
. . . b . . .

Parsed horizontal line:
[cell(1,7-5,1/2),cell(1,7-5,2/2)].
. . . . . . .
. . . . . . .
. b b . . . .
. . . . . . .
. . . . . . .

Parsed horizontal line:
[cell(1,7-5,4/2),cell(1,7-5,5/2)].
. . . . . . .
. . . . . . .
. . . . b b .
. . . . . . .
. . . . . . .

true.
==

Note that while the line segments parsed by the learned grammar are
correctly oriented and positioned in the two-dimensional image plane
(e.g. the first horizontal line is to the left of the vertical line)
this information is not explicitly represented in the grammar, which is
why a second, drawing step is needed to pin down the way the lines
articulate, as expalined above.

*/


%!	write_segmentation_grammar(+Rules) is det.
%
%	Write a learned grammar to an output file.
%
write_segmentation_grammar(Rs):-
	vision_thing_config:line_segmentation_grammar(P)
	,S = open(P,write,Str,[])
	,G = (format(Str,':-module(lines, [lines//2]).~n~n',[])
	     ,format(Str,':-use_module(\'../line_segmentation.pl\').~n~n',[])
	     ,forall(member(R,Rs)
		    ,(copy_term(R,R_)
		     ,numbervars(R_)
		     ,write_term(Str,R_,[fullstop(true)
					 ,nl(true)
					 ,numbervars(true)
					 ,quoted(true)
					])
		     )
		    )
	     )
	,C = close(Str)
	,setup_call_cleanup(S,G,C).



% McCarthyite constraint - excludes left-recursive metasubstitutions
% Allows for invented predicates. Does not take into account existentially
% quantified secod-order variables in metarules.
configuration:metarule_constraints(M,fail):-
	M =.. [m,Id,P|Ps]
	,Id \= projection
	,left_recursive(P,Ps).

left_recursive(T,[T|_Ps]):-
	!.
left_recursive(T,[T,T|_Ps]):-
	!.

:- auxiliaries:set_configuration_option(max_invented, [3]).
:- auxiliaries:set_configuration_option(unfold_invented, [true]).

configuration:double_chain metarule 'P(x,y,a,b):- Q(x,z,a,c), R(z,y,c,b)'.
configuration:double_identity metarule 'P(x,y,a,b):- Q(x,y,a,b)'.

background_knowledge(_/4,[vertical_line/4
                         ,horizontal_line/4
                         ,single_point/4
                         ]).

metarules(_/4,[double_chain,double_identity]).

positive_example(lines/4,E):-
% Learn a shape-agnostic parser.
        image(_S,Is)
        ,image_scan(Is,Ss)
        ,objects(Ss,Os)
        ,member(Cs, Os)
        ,foreground_object(Cs)
        % Don't leave the accumulator unbound on entry
        % or you won't find the exit. EVER!
        %,E =.. [S,[],Cs,[Cs],[[]]].
        ,E =.. [lines,[],[_L|_Ls],[Cs],[[]]].
positive_example(S/4,E):-
% Learn to parse specific shapes.
        S \= lines
	,image(S,Is)
        ,image_scan(Is,Ss)
        ,objects(Ss,Os)
        ,member(Cs, Os)
        ,foreground_object(Cs)
        ,E =.. [S,[],[_L|_Ls],[Cs],[[]]].


negative_example(_/4,_E):-
% No real use of negative examples for segmentation grammars.
        fail.


% ========================================
% Background knowledge definitions - shape segmentation grammar


%!      vertical_line(+Acc,-Line)// is nondet.
%
%       Consume a vertical line from the input.
%
%       As vertical_line// but also accumulates the cells of the
%       matched object.
%
vertical_line(Ls1,[vl(Ls2)|Ls1]), [Rs] -->
        [Cs]
        ,{ vertical_line(Cs,Ls2,Rs)
          ,length(Ls2,N)
          ,N > 1
         }.

%!      horizontal_line(+Acc,-Line)// is nondet.
%
%       Consume a horizontal line from the input.
%
%       As horizontal_line// but also accumulates the cells of the
%       matched object.
%
horizontal_line(Ls1,[hl(Ls2)|Ls1]), [Rs] -->
        [Cs]
        ,{ horizontal_line(Cs,Ls2,Rs)
          ,length(Ls2,N)
          ,N > 1
         }.

%!      single_point(+Acc,-Point)// is nondet.
%
%       Consume a single point from the input.
%
%       As point// but also accumulates the cells of the
%       matched object.
%
single_point([],[pt([C])]), [[]] --> [[C]].


% ========================================
% Background knowledge definitions - basic shapes


%!      single_point(+Cells,-Point,-Rest) is det.
%
%       True when a list of Cells includes a Point.
%
%       Cells is a list of contiguous cells of the same colour,
%       representing an object extracted from an image.
%
%       Point is a subset of Cells representing a single point.
%
%       Rest is the subset of Cells excluding the cells in Point.
%
%       Note that for an objec to be a point it must comprise a single
%       cell, therefore, for this predicate to be true, the length of
%       Cells and Point must be exactly 1 and Rest must be empty.
%
single_point([C],[C],[]).



%!      vertical_line(+Cells,-Line,-Rest) is det.
%
%       True when a list of Cells includes a vertical line.
%
%       Cells is a list of contiguous cells of the same colour,
%       representing an object extracted from an image.
%
%       Line is a subset of Cells representing a vertical line.
%
%       Rest is the the subset of Cells excluding the cells in Line.
%
vertical_line(Cs,Ls,Rs):-
        vertical_line(Cs,[],Ls,[],Rs).

%!      vertical_line(+Cells,-Line,+Acc,-Rest) is det.
%
%       Business end of vertical_line/3.
%
vertical_line([C2],[C1],[C1,C2],Rs_Acc,Rs):-
        reverse(Rs_Acc,Rs)
        ,!.
vertical_line([C],Ls_Acc,Ls,Rs_Acc,Rs):-
        reverse([C|Ls_Acc],Ls)
        ,reverse(Rs_Acc,Rs)
        ,!.
vertical_line([C1,C2|Cs],Ls_Acc,Ls,Rs_Acc,Rs):-
        south(C1,C2)
        ,!
        ,vertical_line([C2|Cs],[C1|Ls_Acc],Ls,Rs_Acc,Rs).
vertical_line([C1,C2|Cs],Ls_Acc,Ls,Rs_Acc,Rs):-
        vertical_line([C1|Cs],Ls_Acc,Ls,[C2|Rs_Acc],Rs).



%!      horizontal_line(+Cells,-Line,-Rest) is det.
%
%       True when a list of Cells includes a horizontal Line.
%
%       Cells is a list of contiguous cells of the same colour,
%       representing an object extracted from an image.
%
%       Line is a subset of Cells representing a horizontal line.
%
%       Rest is the the subset of Cells excluding the cells in Line.
%
horizontal_line(Cs,Ls,Rs):-
        horizontal_line(Cs,[],Ls,[],Rs).

%!      horizontal_line(+Cells,-Line,+Acc,-Rest) is det.
%
%       Business end of horizontal_line/3.
%
horizontal_line([C2],[C1],[C1,C2],Rs_Acc,Rs):-
        reverse(Rs_Acc,Rs)
        ,!.
horizontal_line([C],Ls_Acc,Ls,Rs_Acc,Rs):-
        reverse([C|Ls_Acc],Ls)
        ,reverse(Rs_Acc,Rs)
        ,!.
horizontal_line([C1,C2|Cs],Ls_Acc,Ls,Rs_Acc,Rs):-
        east(C1,C2)
        ,!
        ,horizontal_line([C2|Cs],[C1|Ls_Acc],Ls,Rs_Acc,Rs).
horizontal_line([C1,C2|Cs],Ls_Acc,Ls,Rs_Acc,Rs):-
        horizontal_line([C1|Cs],Ls_Acc,Ls,[C2|Rs_Acc],Rs).
