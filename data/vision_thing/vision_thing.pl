:-module(vision_thing, [background_knowledge/2
                       ,metarules/2
                       ,positive_example/2
                       ,negative_example/2
                        % Background knowledge productions
                       ,vertical_line//0
                       ,horizontal_line//0
                       ,single_point//0
                        % Elementary shapes
                       ,vertical_line/3
                       ,horizontal_line/3
                       ,single_point/3
                        % Cardinal directions
                       ,north/2
                       ,north_east/2
                       ,east/2
                       ,south_east/2
                       ,south/2
                       ,south_west/2
                       ,west/2
                       ,north_west/2
                       ,here/2
                       ]).

:-user:use_module(rendering).
:-user:use_module(image).
:-user:use_module(image_data).
:-use_module(configuration).

/** <module> Object identification in an ARC dataset image.

This experiment file is part of preliminary work on the Abstract
Reasoning Corpus (ARC) dataset introduced in "On the Measure of
Intelligence" (Francois Chollet 2019, available on arxiv). In particular
the MIL problems defined in this experiment file are meant to establish
what the ARC dataset calls "objectness priors", which in MIL terms means
background knowledge of what constitutes an object and its properties.

Progress so far
===============

The following hypotheses of shapes can be learned with the current
version of this experiment file with the listed queries and assuming
the listed configuration options:

==
?- _Ts = [line/2,cross/2,square/2,rectangle/2,point/2], member(_T,_Ts), learn_dynamic(_T,_Ps), reduce_unfolded(_Ps,_Rs), print_clauses(_Rs).
line(A,B):-horizontal_line(A,B).
line(A,B):-vertical_line(A,B).
true ;
cross(A,B):-vertical_line(A,C),horizontal_line(C,D),horizontal_line(D,B).
true ;
square(A,B):-horizontal_line(A,C),horizontal_line(C,B).
square(A,B):-vertical_line(A,C),vertical_line(C,B).
true ;
rectangle(A,B):-horizontal_line(A,C),horizontal_line(C,B).
rectangle(A,B):-vertical_line(A,C),rectangle(C,B).
true ;
point(A,B):-single_point(A,B).
true.

?- list_config.
example_clauses(call)
experiment_file(data/vision_thing/vision_thing.pl,vision_thing)
learner(louise)
max_invented(1)
minimal_program_size(2,inf)
recursion_depth_limit(dynamic_learning,500)
recursive_reduction(false)
reduction(none)
resolutions(5000)
symbol_range(predicate,[P,Q,R,S,T])
symbol_range(variable,[X,Y,Z,U,V,W])
theorem_prover(resolution)
unfold_invented(true)
true.

?- list_problem_statistics(_).
Positive examples:    2
Negative examples:    0
Background knowledge: 3 [vertical_line/2,horizontal_line/2,single_point/2]
Metarules:            2 [chain,identity]
true.
==

Comments on current progress
----------------------------

Shapes are learned as Context-Free Grammars (CFGs) in Definite Clause
Grammars (DCG) notation, which is nice because we get information about
a shape's composition. On the other hand, we get very poor information
about the relative position and properties of the components of a shape
(i.e. all the lines that make it up). Although the very strong bias
imposed on the orientation of objects distinguishes different shapes,
to some extent, we will surely want to determine more information about
the composition of a shape further down the line- which unfortunately
will require a rather more complex grammar to be learned (and
background knowledge to be defined). Eventually, it might make more
sense to abandon the nice grammar paradigm and switch to e.g. a plan
(e.g. "to draw a line, draw a vertical line or a horizontal line"), or a
more general program-as-representation.


Fundamental assumptions
=======================

The image representation and backround knowledge defined for this
experiment file (and in the accompanying source files) embodies a set of
assumptions about objects and their properties. These assumptions are
stated explicitly below.


1. Object cohesion

"An object is a set of contiguous cells of the same colour".

This assumption follows directly from the ARC dataset's first
"objectness prior", i.e. object cohesion based on spatial and colour
continuity.

2. Orientation bias

"Cells in images are ordered left-to-right and top-to-bottom"

A strict ordering bias is imposed on the representation of images and
the objectes therein. An image is a Cartesian plane flipped about the x
axis, i.e. a square grid with the origin at (0,0) and a limit at (x,y),
where x and y are the width and height dimensions of the image.

Cells comprising an object are ordered according to their appearance in
the image grid, so that the first cell of an object is the one closest
to the origin and the last the one closest to its limit.

This assumption is enforced by the image segmentation code in image.pl,
specifically image_scan/2.

3. Background concept

"The background of an image is itself an object (or more than one)".

Some ARC tasks require the background of an image to be manipulated and
transformed so a concept of "background" is likely to be useful. The
background of an image is composed of cells of the same colour and is
divided into contiguous sub-objects (i.e. regions) surrounding
foreground objects.

This assumption too follows from the ARC dataset's first objectness
prior, which includes the ability to parse an image into zones.

4. Properties of objects

"Objects have distinct shapes and colours".

The most important property of an object is that it can described in
terms of its shape. The second most important property is that it can be
described in terms of its colour. This is an immediate consequence of
the first fundamental assumption.

5. Shapes are made of lines

"In a two-dimensional grid, each shape is composed of straight lines".

Every shape that can be represented in a two-dimensional grid can be
represented by a suitable arrangment of single-width lines drawn between
two points on the grid.

Background knowledge defined in this experiment file describes
elementary line types such as vertical lines, horizontal lines, single
points, etc. These low-level elements are used to construct more complex
objects, such as squares, rectangles, and so on.


Learning object representations
-------------------------------

While the first three of the fundamental assumptions described above are
hard-coded and hand-crafted, the properties of objects are to be
learned. The MIL problems defined in this experiment file are designed
to train Louise with examples of objects of specific shapes, in order to
learn representations of those shapes.

In the current version of this experiment file, shape representations
are learned in the form of Context-Free Grammars, in Prolog's native
Definite Clause Grammars notation. Each rule in the grammar represents
an object as a composite of the basic rules defining elementary lines,
e.g. vertical_line//0, horizontal_line//0 etc. These rules in turn are
defined in terms of ordinary (non-grammar) predicates reprsenting
cardinal directions and primitive lines and points.

For example, the following is a representation of a cross shape learned
by Louise from the MIL problem defined in this experiment file:

==
cross(A,B):-vertical_line(A,C),horizontal_line(C,D),horizontal_line(D,B).
==

This representation is equivalent to the following DCG production:

==
cross --> vertical_line, horizontal_line, horizontal_line.
==

The examples from which the cross/2 representation above was learned are
objects extracted from the following images (defined in image_data.pl):

==
[[0,1,0]
,[1,1,1]
,[0,1,0]]

[[0,0,0,1,0,0,0]
,[0,0,0,1,0,0,0]
,[0,1,1,1,1,1,0]
,[0,0,0,1,0,0,0]
,[0,0,0,1,0,0,0]]
==

The images above are in the project's internal representation of an
image grid, as a list-of-lists, where sublists represent rows in the
image and their elements are numbers representing cells of distinct
colours. The following are the portrayals of the two images above, using
portray_object/3 ("b" is the colour blue; a dot is a stand-in for a
position in the grid not occupied by a cell inside an object):

==
. b .
b b b
. b .

. . . b . . .
. . . b . . .
. b b b b b .
. . . b . . .
. . . b . . .
==

The current representation is way too basic and is missing important
information, e.g. a cross is described as a vertical line, followed by
two horizontal lines, but this is not enough to describe a cross without
some information about the articulation of the components of the shape.

Note also that the above theory fails to cover the first cross shape
above, which should be described as a vertical line and two signle
points.

This documentation section will be updated regularly with the current
state of the project's completion.

*/

:- auxiliaries:set_configuration_option(max_invented, [1]).
:- auxiliaries:set_configuration_option(unfold_invented, [true]).

background_knowledge(_/2,[vertical_line/2
                         ,horizontal_line/2
                         ,single_point/2
                         ]).

metarules(_/2,[chain,identity]).

positive_example(S/2,E):-
        image(S,Is)
        ,image_scan(Is,Ss)
        ,objects(Ss,Os)
        ,member(Cs, Os)
        ,foreground_object(Cs)
        ,E =.. [S,[Cs],[[]]].

negative_example(_/2,_E):-
        fail.

% ========================================
% Background knowledge definitions - shape grammars

%!      vertical_line// is nondet.
%
%       Consume a vertical line from the input.
%
vertical_line, [Rs] -->
        [Cs]
        ,{ vertical_line(Cs,Ls,Rs)
          ,length(Ls,N)
          ,N > 1
         }.

%!      horizontal_line// is nondet.
%
%       Consume a horizontal line from the input.
%
horizontal_line, [Rs] -->
        [Cs]
        ,{ horizontal_line(Cs,Ls,Rs)
          ,length(Ls,N)
          ,N > 1
         }.

%!      single_point// is nondet.
%
%       Consume a single point from the input.
%
single_point, [[]] --> [[_C]].



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



% ========================================
% Background knowledge definitions - orientation primitives


%!      north(+Cell1,-Cell2) is det.
%
%       True when Cell2 is north of Cell1.
%
north(cell(C,D,P1),cell(C,D,P2)):-
        move(P1,D,-,0/1,P2).

%!      north_east(+Cell1,-Cell2) is det.
%
%       True when Cell2 is north-east of Cell1.
%
north_east(cell(C,D,P1),cell(C,D,P2)):-
        move(P1,D,+,1/0,P_)
        ,move(P_,D,-,0/1,P2).

%!      east(+Cell1,-Cell2) is det.
%
%       True when Cell2 is east of Cell1.
%
east(cell(C,D,P1),cell(C,D,P2)):-
        move(P1,D,+,1/0,P2).

%!      south_east(+Cell1,-Cell2) is det.
%
%       True when Cell2 is south-east of Cell1.
%
south_east(cell(C,D,P1),cell(C,D,P2)):-
        move(P1,D,+,1/0,P_)
        ,move(P_,D,+,0/1,P2).

%!      south(+Cell1,-Cell2) is det.
%
%       True when Cell2 is south of Cell1.
%
south(cell(C,D,P1),cell(C,D,P2)):-
        move(P1,D,+,0/1,P2).

%!      south_west(+Cell1,-Cell2) is det.
%
%       True when Cell2 is south-west of Cell1.
%
south_west(cell(C,D,P1),cell(C,D,P2)):-
        move(P1,D,-,1/0,P_)
        ,move(P_,D,+,0/1,P2).

%!      west(+Cell1,-Cell2) is det.
%
%       True when Cell2 is west of Cell1.
%
west(cell(C,D,P1),cell(C,D,P2)):-
        move(P1,D,-,1/0,P2).

%!      north_west(+Cell1,-Cell2) is det.
%
%       True when Cell2 is north-west of Cell1.
%
north_west(cell(C,D,P1),cell(C,D,P2)):-
        move(P1,D,-,1/0,P_)
        ,move(P_,D,-,0/1,P2).

%!      here(+Cell1,-Cell2) is det.
%
%       True when Cell1 and Cell2 are the same cell.
%
here(C,C).


%!      move(+Current,+Delta,+Distance,-New) is det.
%
%       Make a move from a Current cell.
%
%       As move/5 in image.pl but does not automatically modify the
%       width and height dimensions to change rows!
%
move(X/Y,W-H,D,Dx/Dy,Ex/Ey):-
	ground(X/Y)
	,ground(Dx/Dy)
	,ground(D)
	,Mv_x =.. [D,X,Dx]
	,Mv_y =.. [D,Y,Dy]
	,Ex is Mv_x
	,Ey is Mv_y
	,within_limits(Ex,W)
	,within_limits(Ey,H).
