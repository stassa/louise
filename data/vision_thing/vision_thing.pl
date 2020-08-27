:-module(vision_thing, [% Cardinal directions
                        north/2
                       ,north_east/2
                       ,east/2
                       ,south_east/2
                       ,south/2
                       ,south_west/2
                       ,west/2
                       ,north_west/2
                       ,here/2
                       ,move/5
                       ]).

:-user:use_module(rendering).
:-user:use_module(image).
:-user:use_module(image_data).
:-user:use_module(debugging).
:-use_module(configuration).

/** <module> Object identification in an ARC dataset image.

Table of contents
=================

The following is the table of contents for this documentation section.
Search for the tags in square brackets to jump to each section. Repeat
the search to jump back to the TOC.

1. Project outline [project_outline]
2. Directory structure [directory_structure]
3. Fundamental assumptions [fundamental_assumptions]
4. Learning representations [learning_representations]
5. Progress so far [current_progress]


Project outline
===============
[project_outline]

This file is the entry point for the "Vision Thing" project, undertaken
to complete preliminary work on the Abstract Reasoning Corpus (ARC)
dataset introduced in "On the Measure of Intelligence" (Francois Chollet
2019: https://arxiv.org/abs/1911.01547). In particular the MIL problems
defined in the Vision Thing project are meant to establish what the ARC
dataset calls "objectness priors", which in MIL terms means background
knowledge of what constitutes an object and its properties.

"Vision Thing" comprises a number of separate experiment files, each
defining a particular task in an image analysis pipeline that, once
completed, will represent an ARC dataset problem as an image
transformation problem. The idea is to analyse the two images comprising
an ARC problem, find their differences and find a plan to transform the
input image to the output image. Individual tasks are either
hand-crafted Prolog programs, or MIL problems set up to learn classic AI
representations, such as grammars and plans.

Directory structure
===================
[directory_structure]

Vision Thing comprises the following source files:

louise/data/vision_thing
|
|   debugging.pl
|   image.pl
|   image_data.pl
|   line_segmentation.pl
|   rendering.pl
|   rendering_config.pl
|   shape_drawing.pl
|   vision_thing.pl
|
\---output

1. debugging.pl

   Debugging facilities and visualisation for learned hypotheses.

2. image.pl

   Partition an image into objects.

3. image_data.pl

   Examples of raw images in the ARC dataset's format.

4. line_segmentation.pl

   Experiment file set up to learn line segmentation grammars.

5. rendering.pl

   Predicates to visualise images and objects in glorious ASCII.

6. rendering_config.pl

   Configuration options for rendering.pl

7. shape_drawing.pl

   Experiment file set up to learn shape drawing plans.

8. vision_thing.pl

   Project entry point- this file.


Fundamental assumptions
=======================
[fundamental_assumptions]

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
   the objectes therein. An image is a Cartesian plane flipped about the
   x axis, i.e. a square grid with the origin at (0,0) and a limit at
   (x,y), where x and y are the width and height dimensions of the
   image.

   Cells comprising an object are ordered according to their appearance
   in the image grid, so that the first cell of an object is the one
   closest to the origin and the last the one closest to its limit.

   This assumption is enforced by the image segmentation code in
   image.pl, specifically image_scan/2.

3. Background concept

   "The background of an image is itself an object (or more than one)".

   Some ARC tasks require the background of an image to be manipulated
   and transformed so a concept of "background" is likely to be useful.
   The background of an image is composed of cells of the same colour
   and is divided into contiguous sub-objects (i.e. regions) surrounding
   foreground objects.

   This assumption too follows from the ARC dataset's first objectness
   prior, which includes the ability to parse an image into zones.

4. Properties of objects

   "Objects have distinct shapes and colours".

   The most important property of an object is that it can described in
   terms of its shape. The second most important property is that it can
   be described in terms of its colour. This is an immediate consequence
   of the first fundamental assumption.

5. Shapes are made of lines

   "In a two-dimensional grid, each shape is composed of straight
   lines".

   Every shape that can be represented in a two-dimensional grid can be
   represented by a suitable arrangment of single-width lines drawn
   between two points on the grid.

   Background knowledge defined in this experiment file describes
   elementary line types such as vertical lines, horizontal lines,
   single points, etc. These low-level elements are used to construct
   more complex objects, such as squares, rectangles, and so on.

6. Object relations
   "An Image is a set of objects participating in relations".

    An image is a _gestalt_ of objects relating to each other in a
    specific manner. Two images are different not only because they
    contain different objects with different properties but also because
    the objects in each image are in different relations with each
    other.


Learning representations
------------------------
[learning_representations]

For every concept that must be reprsented to solve the problems in the
ARC dataset, the decision has to be made of whether to learn it or
whether to provide a representation of the concept already hand-crafted.
This is in particular the case with respect to background knowledge,
which is the set of building blocks of ARC problem solutions we hope to
provide.

In general, we choose to learn representations of a concept if the
concept is likely to have many variants. Otherwise, we hand-craft
a representation of a concept.

For example, we provide hand-crafted definitions of grammar productions
representing a vertical line, to be used as background knowledge for the
task of learning line segmentation grammars. The concept of a "vertical
line" never changes, in any image, for any object: it is always a line
oriented from north to south. On the other hand, the concept of a set of
line segments comprising an object changes substantially depending on
the shape of the object. Accordingly, we learn line segmentation
grammars from examples, rather than hand-craft them.

Note that what changes and what remains the same itself depends on the
bedrock of fundamental assumptions we make about an image. For example,
the concept of a vertical line depends on the orientation bias and our
representation of the cardinal and ordinal directions in an image as
hand-crafted background knowledge. It wouldn't make sense to describe a
vertical line in terms of direction, unless we had already defined the
concept of directions- or, attempted to learn it. However, the concept
of direction is itself unchanging so there is no need to learn it from
scratch.


Progress so far
===============
[current_progress]

In Summary
----------

For the time being we are concentrating on learning representations of
an object's shape in the fomr of a grammar and a plan describing how to
draw the shape as a set of contiguous lines (and points).

More analytically
-----------------

The most important properties of an object are its shape and colour. The
colour of an object can easily be found by looking at any of its cells.
Representing an object's shape, for objects of arbitrary shapes, is more
complicated. The purpose of the work done so far is to teach Louise
representations of objects' shapes.

In short, we teach Louise to represent an object's shape in two steps.

In the first step, a "line segmentation grammar" is learned that shows
how an object is composed of vertical and horizontal lines or single
points.

Line segmentation grammars analyse an object into its component lines,
but do not explain how those lines are articulated to compose the
object's shape. The former task is handled by the second step, a "shape
drawing plan", composed of read and write operations that move a
"read/write head" over the shape, writing lines and points as required.

The two tasks are defined separately as two experiment files,
line_segmentation.pl and shape_drawing.pl, each with its own background
knowledge. The line segmentation grammars learned from the MIL problems
defined in line_segmentation.pl are added to the BK of shape_drawing.pl.

Current progress in learning line segementation grammars
--------------------------------------------------------

The following line segmentation grammars can be learned with the current
version of line_segmentation.pl, with the listed queries and assuming
the listed configuration options and MIL problem elements:

==
?- list_config.
example_clauses(call)
experiment_file(data/vision_thing/line_segmentation.pl,line_segmentation)
learner(louise)
max_invented(3)
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

?- list_problem_statistics(_/4).
Positive examples:    24
Negative examples:    0
Background knowledge: 3 [vertical_line/4,horizontal_line/4,single_point/4]
Metarules:            2 [double_chain,double_identity]
true.

?- _N = 4, _Ts = [line,cross,square,rectangle,point], member(_T,_Ts), learn_dynamic(_T/_N,_Ps), reduce_unfolded(_Ps,_Rs), print_clauses(_Rs).%, experiment_data(_T/_N,_Pos,_Neg,_BK,_MS), debug_learned(_Pos, _BK, _Rs).
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

Current progress in learning shape drawing plans
------------------------------------------------

The following shape drawing plans can be learned with the current
version of line_segmentation.pl, with the listed queries and assuming
the listed configuration options and MIL problem elements:

==
?- list_config.
example_clauses(call)
experiment_file(data/vision_thing/shape_drawing.pl,shape_drawing)
learner(louise)
max_invented(2)
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

?- list_problem_statistics(_/2).
Positive examples:    46
Negative examples:    0
Background knowledge: 7 [write_vertical_line/2,write_horizontal_line/2,write_point/2,read_north/2,read_south/2,read_east/2,read_west/2]
Metarules:            2 [chain,identity]
true.

?- _N = 2, _Ts = [line,cross,square,rectangle,point], member(_T,_Ts), learn_dynamic(_T/_N,_Ps), reduce_unfolded(_Ps,_Rs), print_clauses(_Rs).
line(A,B):-write_vertical_line(A,B).
line(A,B):-write_horizontal_line(A,B).
true ;
[]
true ;
square(A,B):-write_horizontal_line(A,C),read_west(C,D),read_south(D,E),write_horizontal_line(E,B).
true ;
[]
true ;
point(A,B):-write_point(A,B).
true.
==


Comments on current progress
----------------------------

The first task in the pipeline, learning line segmentation grammars, is
relatively easy and requires relatively few background predicates.

The second task, learning shape drawing plans is a bit trickier and
requires more background knowledge to represent read/write operations.
Currently only simple line and square shapes are learned, mostly because
the BK defined so far is not sufficient to learn anything more (or
invent the missing BK).

It is interesting to note that a line segmentation grammar provides
_some_ information about the articulation of the component lines of an
object. That is thanks to the very strong orientation bias imposed by
the hand-crafted object extraction code in image.pl. It is certainly
possible to reproduce a specific object of a given shape given only a
line segmentation grammar that can successfully recognise the object, by
examining the parsed lines in the object to find out the starting cell
for each such line. However, this representation will not generalise
very well to arbitrary objects of a given shape. Drawing plans, learned
from sufficiently varied examples, should not have this over-specific
problem.

Note also that it is possible to learn a _grammar_ to represent a
drawing plan. However, such a grammar would have productions with a very
complex structure, to represent the relevant components of the drawing
program's state. While Louise _can_ learn such a grammar, setting up a
MIL problem to learn it would require defining by hand very specific
metarules and extensive background knowledge. This is something that we
would like to avoid - in general, we want to keep the hand-crafting of
background knowledge and metarueles to a minimum.

*/


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
