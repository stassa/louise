:-module(vision_thing, [background_knowledge/2
                       ,metarules/2
                       ,positive_example/2
                       ,negative_example/2
                        % Grammar productions
                       ,go_north/2
                       ,go_north_east/2
                       ,go_east/2
                       ,go_south_east/2
                       ,go_south/2
                       ,go_south_west/2
                       ,go_west/2
                       ,go_north_west/2
                       ,stop/2
                        % Cardinal moves
                       ,north/2
                       ,north_east/2
                       ,east/2
                       ,south_east/2
                       ,south/2
                       ,south_west/2
                       ,west/2
                       ,north_west/2
                       ,here/2
                       ,move/4
                       ]).

:-user:use_module(rendering).
:-user:use_module(image).
:-user:use_module(image_data).

/** <module> Object identification in an ARC dataset image.


This is part of preliminary work on the ARC dataset first presented in
"On the Measure of Intelligence" (Francois Chollet 2019, available on
arxiv). In particular the experiments in this experiment file are meant
to establish what the ARC dataset calls "objectness priors", which in
MIL terms means background knowledge of what constitutes an object and
its properties.

Fundamental assumptions
=======================

object cohesion
---------------

Images in the ARC dataset are represented as grids of single integers
representing colours. In our representations we implement image grids as
lists-of-lists of integers. A fundamental assumption of our
representation is that images are composed of _objects_ that are
identified as sets of contiguous cells of the same colour.

In our representation the background of an image (which is always the
same colour, black, in the ARC dataset) is itself considered an object,
or even multiple objects when other objects separate the background's
cells so that they are not all contiguous. This might seem
counter-intuitive, but the background itself must be manipulated to
solve some problems so it's useful to treat it in the same way as other
objects (that must also be manipulated).

The object cohesion assumption is hard-coded in object extraction logic
in image.pl. Predicates in that source file scan an image to collect
sets of contiguous cells of the same colour and return them as lists of
cell/2 compound terms each of which represents an individual cell with a
colour and x/y coordinates in the image.

Object properties
-----------------

Objects have properties that correspond to objectness priors as
described in the paper introducing the ARC dataset. The first of those
objectness priors is that objects have distinct shapes that belong to
general categories, e.g. lines, squares, "circles", etc. Other important
properties are a shape's length, colour and position relative the
background and other objects.

The purpose of this experiment file is to learn representations of
object shapes from examples of objects and background knowledge
describing operations for object construction such as predicates to
move a "write head" towards different directions etc. Louise is then
used to learn grammars describing shapes in terms of the object
construction operators.

Object and image orientation
----------------------------

A strict ordering bias is imposed on the representation of objects and
images. An image has an origin at (0,0) in the upper-left corner and
ends at (x,y) in the lower-right corner where x and y are the width and
height dimensions of the image.

Accordingly the cells comprising an object are ordered from
left-to-right and top-to-bottom, so that the first cell in an object is
the cell with the topmost, leftmost coordinates and the last cell in the
object is the one with the bottom-most, rightmost coordinates.

This fundamental assumption constitues an "orientation prior", in other
words we assume that objects are oriented in the 2-dimensional space
defined by an image. This can be useful when reasoning about
transformations of objects between images, e.g. we can figure out that
a line in one image is rotated with respect to a line in another image
etc.

*/

:- auxiliaries:set_configuration_option(max_invented, [3]).
:- auxiliaries:set_configuration_option(unfold_invented, [true]).

configuration:metarule_constraints(M,fail):-
% Force stop/2 to be used only at the end of a clause except when it is
% the only constituent or the target is a single point.
        M =.. [m,_Id,P,Q|Ps]
        ,Q = stop
        ,(   Ps \= []
         ;   P \= point
         ).

configuration:metarule_constraints(M,fail):-
% Avoid clauses of the form shape(A,B):- shape(A,C),stop(C,B)
        M =.. [m,_Id,P,P,stop].

% McCarthyite constraint - excludes left-recursive metasubstitutions
% Allows for invented predicates. Does not take into account existentially
% quantified secod-order variables in metarules.
configuration:metarule_constraints(M,fail):-
	M =.. [m,Id,P|Ps]
	,Id \= projection
	,left_recursive(P,Ps).

left_recursive(T,[T|_Ps]):-
	!.
left_recursive(T,[T,T|_Ps]).

background_knowledge(_/2,[go_north/2
                         ,go_north_east/2
                         ,go_east/2
                         ,go_south_east/2
                         ,go_south/2
                         ,go_south_west/2
                         ,go_west/2
                         ,go_north_west/2
                         ,stop/2
                         ]).

metarules(_/2,[chain,identity]).

positive_example(S/2,E):-
        image(S,Is)
        ,image_scan(Is,Cs)
        ,objects(Cs,Os)
        ,member(O, Os)
        ,foreground_object(O)
        ,append(_H,[T],O)
        ,E =.. [S,O,[T]].

negative_example(_/2,_):-
        fail.

go_north, [C2] --> [C1,C2], { north(C1,C2) }.
go_north_east, [C2] --> [C1,C2], { north_east(C1,C2) }.
go_east, [C2] --> [C1,C2], { east(C1,C2) }.
go_south_east, [C2] --> [C1,C2], { south_east(C1,C2) }.
go_south, [C2] --> [C1,C2], { south(C1,C2) }.
go_south_west, [C2] --> [C1,C2], { south_west(C1,C2) }.
go_west, [C2] --> [C1,C2], { west(C1,C2) }.
go_north_west, [C2] --> [C1,C2], { north_west(C1,C2) }.
stop, [C] --> [C], { here(C,C) }.


%!      north(+Cell1,-Cell2) is det.
%
%       True when Cell2 is north of Cell1.
%
north(cell(C,P1),cell(C,P2)):-
        move(P1,-,0/1,P2).

%!      north_east(+Cell1,-Cell2) is det.
%
%       True when Cell2 is north-east of Cell1.
%
north_east(cell(C,P1),cell(C,P2)):-
        move(P1,+,1/0,P_)
        ,move(P_,-,0/1,P2).

%!      east(+Cell1,-Cell2) is det.
%
%       True when Cell2 is east of Cell1.
%
east(cell(C,P1),cell(C,P2)):-
        move(P1,+,1/0,P2).

%!      south_east(+Cell1,-Cell2) is det.
%
%       True when Cell2 is south-east of Cell1.
%
south_east(cell(C,P1),cell(C,P2)):-
        move(P1,+,1/0,P_)
        ,move(P_,+,0/1,P2).

%!      south(+Cell1,-Cell2) is det.
%
%       True when Cell2 is south of Cell1.
%
south(cell(C,P1),cell(C,P2)):-
        move(P1,+,0/1,P2).

%!      south_west(+Cell1,-Cell2) is det.
%
%       True when Cell2 is south-west of Cell1.
%
south_west(cell(C,P1),cell(C,P2)):-
        move(P1,-,1/0,P_)
        ,move(P_,+,0/1,P2).

%!      west(+Cell1,-Cell2) is det.
%
%       True when Cell2 is west of Cell1.
%
west(cell(C,P1),cell(C,P2)):-
        move(P1,-,1/0,P2).

%!      north_west(+Cell1,-Cell2) is det.
%
%       True when Cell2 is north-west of Cell1.
%
north_west(cell(C,P1),cell(C,P2)):-
        move(P1,-,1/0,P_)
        ,move(P_,-,0/1,P2).

%!      here(+Cell1,-Cell2) is det.
%
%       True when Cell1 and Cell2 are the same cell.
%
here(C,C).


%!      move(+Current,+Delta,+Distance,-New) is det.
%
%       Make a move from a Current cell.
%
%       As move/5 but does not take into account the limits of the
%       image.
%
move(X/Y,D,Dx/Dy,Ex/Ey):-
	ground(X/Y)
	,ground(Dx/Dy)
	,ground(D)
	,Mv_x =.. [D,X,Dx]
	,Mv_y =.. [D,Y,Dy]
	,Ex is Mv_x
	,Ey is Mv_y.
