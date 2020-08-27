:-module(shape_drawing, [background_knowledge/2
                        ,metarules/2
                        ,positive_example/2
                        ,negative_example/2
                         % Shape drawing primitives
                        ,write_vertical_line/2
                        ,write_horizontal_line/2
                        ,write_point/2
                        ,read_north/2
                        ,read_south/2
                        ,read_east/2
                        ,read_west/2
			,start/2
			,end/2
                        ]).

:-use_module(vision_thing).
:-use_module(output/lines).
:-use_module(configuration).

/** <module> Learn plans to draw the shapes of objects in an ARC image.

This experiment file defines MIL problems used to learn plans for
drawing the shapes of objects in raw images. The following is an example
of a plan learned for a very simple square shape:

==
?- learn_dynamic(square/2).
'$1'(A,B):-read_west(A,C),'$2'(C,B).
'$2'(A,B):-read_south(A,C),write_horizontal_line(C,B).
square(A,B):-write_horizontal_line(A,C),'$1'(C,B).
true.
==

Learned plans consist of successive calls to drawing and reading
operations. Drawing operations draw one of the lines comprising a shape.
Reading operations move a virtual "read/write head" to a new position.

Reading and writing operations are represented as dyadic predicates
where the first argument is the state of the drawing program at the
start of the operation and the second argument is the state of the
drawing program at the end of the operation.

States are represented as compounds s(C,Cs,Ls,Ws), where:
* C: Current cell
* Cs: Current line
* Ls: Remaining lines
* Ws: Write buffer

C, the current cell is a cell/3 compound, marking the position of the
"read head" of the shape drawing program we are trying to learn.

Cs, the current line is a list of cells, the line including the current
cell.

Ls, the remaining lines, is a list of lists of cells, the lines
remaining to be drawn. Drawing a shape is not complete until Ls is
empty.

Ws, the write buffer, is a list of lists of cells, the lines drawn so
far. Drawing a shape is not complete until all lines have been moved
from Ls to Ws.

Learning targets for the MIL problems defined in this experiment file
represent either: a) specific shapes, like line, cross, square,
rectangle, etc, or, b) generic objects of arbitrary shape. In both
cases, examples are given as compounds T(S1,S2), where T is the
predicate symbol of the learning target (the name of a shape or "objct",
without "e", for generic objects) and S1, S2 are the start and end
states of a plan for drawing the object represented by the learning
target.

In the case that the learning target is a generic object of arbitrary
shape, the learned plans subsume the drawing plans of all individual
shapes that can be learned from this experiment file. For example, if
separate learning targets for line, cross, square and rectangle shapes
exist, setting a generic object target will direct Louise to learn a
plan able to draw any of the above shapes. In a sense, a generic plan
approximates a general shape drawing strategy (and is more accurate the
more examples of different shapes are given). However, such a plan
can grow to be very, very large (several tens of thousands clauses) and
may take a long time to learn. Further, there is a tradeoff between the
generality of an all-encompassing strategy and the specificity provided
by a plan that can draw named shapes, whose names can be used to reason
about them using specific background knowledge (e.g. a "circle" can
encompass other objects, a "triangle" has three corners, etc).

*/

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

:- auxiliaries:set_configuration_option(max_invented, [2]).
:- auxiliaries:set_configuration_option(unfold_invented, [true]).

background_knowledge(_/2,[write_vertical_line/2
			 ,write_horizontal_line/2
			 ,write_point/2
			 ,read_north/2
			 ,read_south/2
			 ,read_east/2
			 ,read_west/2
			 ]).

metarules(_/2,[chain,identity]).

positive_example(S/2,E):-
	pos_ex(S,E).
positive_example(objct/2,objct(S1,S2)):-
       pos_ex(_,E_)
       ,E_ =.. [_,S1,S2].

negative_example(_/2,_E):-
        fail.

%!	pos_ex(?Target,-Example) is nondet.
%
%	Abstracts away collection of examples from images.
%
%	Target is either objct/2 (note the absence of "e") or the name
%	of a shape represented in a raw image in image_data.pl.
%	Appropriate shape names can be retrieved in the first argument
%	of image/2.
%
%	Example is a two-arity atom of Target where the first argument
%	is the starting state and the second the end state of a plan to
%	draw the Target shape, or a generic objct (in which case the
%	plan encompasses every known object shape).
%
%	The learning targets in this experiment file represent plans for
%	drawing objcts with specific shapes, or generic object drawing
%	plans. The examples for those targets are states of a program
%	operating on objects parsed into lines from raw images in
%	image_data.pl. In order to be used as examples, the raw images
%	must first be scanned to identify objects in them, then those
%	objects must be parsed into lines by the line segmentation
%	grammar learned from the MIL problems defined in
%	line_segmentation.pl (and stored in output/lines.pl). Finally,
%	appropriate start and end states for each drawing task for which
%	a plan must be found have to be constructed.
%
%	This predicate abstracts this fairly complicated process so that
%	it doesn't have to be duplicated for each and every learning
%	target defined in this experiment file.
%
%
pos_ex(S,E):-
	image(S,Is)
	,image_scan(Is,Ss)
	,objects(Ss,Os)
	,member(Cs, Os)
	,foreground_object(Cs)
	,once(phrase(lines([],Ls_),[Cs], [[]]))
	,reverse(Ls_,Ls)
	,Ls = [Ls_1|_]
	,start(Ls_1,C)
	,last(Ls,Ls_n)
	,end(Ls_n,C_)
	,S1 = s(C,Ls_1,Ls,[])
	,S2 = s(C_,Ls_n,[],Ls_)
	,E =.. [S,S1,S2]	.



% ========================================
% Background knowledge definitions - shape drawing primitives

%!	write_vertical_line(+Start_State,-End_State) is det.
%
%	Write a vertical line starting at the current position.
%
write_vertical_line(s(C1,vl(Cs_n),[vl(Cs_n)|Ls],Ws)
                   ,s(C_n,vl(Cs_n),Ls,[vl(Cs_n)|Ws])):-
        start(vl(Cs_n),C1)
        ,last(Cs_n,C_n).


%!	write_horizontal_line(+Start_State,-End_State) is det.
%
%	Write a horizontal line starting at the current position.
%
write_horizontal_line(s(C1,hl(Cs_n),[hl(Cs_n)|Ls],Ws)
                   ,s(C_n,hl(Cs_n),Ls,[hl(Cs_n)|Ws])):-
        start(vl(Cs_n),C1)
        ,last(Cs_n,C_n).


%!	write_point(+Start_State,-End_State) is det.
%
%	Write a single point at the current position.
%
write_point(s(_,_,[pt([C_n])|Ls],Ws)
           ,s(C_n,pt([C_n]),Ls,[pt([C_n])|Ws])).


%!	read_north(+Start_State,-End_State) is det.
%
%	Read one cell to the north of the current position.
%
read_north(s(C1,_Cs_1,[Cs_N|Ls],Ws)
          ,s(C2,Cs_N,[Cs_N|Ls],Ws)):-
        start(Cs_N,C2)
        ,north(C1,C2).


%!	read_south(+Start_State,-End_State) is det.
%
%	Read one cell to the south of the current position.
%
read_south(s(C1,_Cs_1,[Cs_S|Ls],Ws)
          ,s(C2,Cs_S,[Cs_S|Ls],Ws)):-
        start(Cs_S,C2)
        ,south(C1,C2).


%!	read_east(+Start_State,-End_State) is det.
%
%	Read one cell to the east of the current position.
%
read_east(s(C1,Cs_1,Ls,Ws)
         ,s(C2,Cs_1,Ls,Ws)):-
        start(Cs_1,C2)
        ,east(C1,C2).


%!	read_west(+Start_State,-End_State) is det.
%
%	Read one cell to the west of the current position.
%
read_west(s(C1,Cs_1,Ls,Ws)
         ,s(C2,Cs_1,Ls,Ws)):-
        start(Cs_1,C2)
        ,west(C1,C2).

%!	start(+Line,?Cell) is det.
%
%	The first Cell of a Line.
%
start(vl([C|_Cs]),C):-
	!.
start(hl([C|_Cs]),C):-
	!.
start(pt([C]),C).


%!	end(+Line,?Cell) is det.
%
%	The last Cell of a Line.
%
end(vl(Cs),C):-
        last(Cs,C)
	,!.
end(hl(Cs),C):-
        last(Cs,C)
	,!.
end(pt([C]),C).


/*
Currently unused start/2 and end/2 versions that also separate a first
or last cell from the rest of the line, so the rest of the line can be
processed in isolation. Not currently used but may come in handy later.

%!	start(?Line,?Cell,?Rest) is semidet.
%
%	Separate the first Cell of a Line from the Rest of the Line.
%
start(vl([C|Cs]),C,Cs).
start(hl([C|Cs]),C,Cs).
start(pt([C]),C,[]).


%!	end(?Line,?Cell,?Rest) is semidet.
%
%	Separate the last Cell of a Line from the Rest of the Line.
%
end(vl(Cs),C,Fs):-
        append(Fs,[C],Cs).
end(hl(Cs),C,Fs):-
        append(Fs,[C],Cs).
end(pt([C]),C,[]).
*/
