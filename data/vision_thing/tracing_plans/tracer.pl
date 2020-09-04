:-module(tracer, [trace_plan/2
                 ]).

:-use_module('../image_data').
:-use_module('../vision_thing').

/** <module> Construct a plan to trace an object's cells.

Predicates in this module construct a "tracing plan", consisting of
navigation instructions to direct a virtual agent in a path tracing the
cells of an object extracted from an image in the ARC dataset format.

Tracing plans created with trace_plan/2 are an alternative to the shape
drawing plans learned by Louise in data/vision_thing/shape_drawing.pl.

Achilles
--------

The tracing agent is in a sense the opposite of the "turtle robot" used
in turtle graphics, therefore we will call this tracing agent
"Achilles", as an allusion to Zeno's paradox of "Achilles and the
Tortoise".

Achilles can only execute a handful of very simple movement instructions
that move it one cell forward towards one of the four cardinal and four
ordinal directions defined in data/vision_thing/vision_thing.pl (N, NE,
E, SE, etc). Additionally, Achilles can perform an unconditional jump to
an arbitrary location within the confines of an image. Finally, Achilles
can read from a list of cells representing an object, called the Input
and also push to and pop an element from a Stack. The need for a stack
is explained immediately following.

The Input and the Stack
-----------------------

The Input is a list of contiguous cells representing an object extracted
from an image in the ARC dataset's format. Objects are extraced from
images by the predicates in data/vision_thing/image.pl. Cells are Prolog
compounds of the form c(C,W-H,X/Y), where C is the colour of a cell in
the image, W and H are the width and height dimensions of the image used
to constraint movements within the image bounds and X and Y are the
coordinates of the cell in the horizontal and vertical axes,
respectively.

Objects are extracted from an image by the predicate objects/2, defined
in data/vision_thing/image.pl. objects/2 returns a list of lists where
each sublist represents an object. In an object-list, cells are ordered
according to their position in the image, from left-to-right and from
top-to-bottom, so that a cell closer to the top-left corner of the
image is found in the list before a cell that is closer to the
bottom-right of the image. This means that two cells that are adjacent
in an object-list are not necessariy adjacent in the original image.

As an example, consider the following representation of a cross-shaped
object, in the ARC dataset format:

==
[[0,0,0,1,0,0,0]
,[0,0,0,1,0,0,0]
,[0,1,1,1,1,1,0]
,[0,0,0,1,0,0,0]
,[0,0,0,1,0,0,0]
]
==

The cells with colour "1" make up the cross-shaped object. objects/2
will return the cross-shaped object in the image above as a list of
cells, as follows:

==
?- image(cross,_Is),image_scan(_Is,_Ss),objects(_Ss,_Os),member(_Cs,_Os), foreground_object(_Cs), portray_object(_Cs).
[cell(1,7-5,3/0),cell(1,7-5,3/1),cell(1,7-5,1/2),cell(1,7-5,2/2),cell(1,7-5,3/2),cell(1,7-5,4/2),cell(1,7-5,5/2),cell(1,7-5,3/3),cell(1,7-5,3/4)].
. . . b . . .
. . . b . . .
. b b b b b .
. . . b . . .
. . . b . . .
true .
==

Above, we use foreground_object/1 to filter out objects composed of
background-coloured cells and visualise the shape using
portray_object/1. The list of cells of the cross-shaped object is
printed before the visualisation of the object's cells.

Note that in the object-list extracted from the raw image by objects/2,
the cells with coordinates 3/0 and 1/2 are adjacent, in the sense that
they are represented by the second and third element in the object-list,
respectively. However, in the object itself, those cells are not
adjacent, in the sense that they are separated by more than one cell in
any of the eight cardinal and ordinal directions.

The result of this incongruity is that when tracing a path through the
elements of a list of cells representing an object, some elements of the
list must be skipped temporarily, until they can be accessed from an
adjacent cell- or, alternatively, jumped-to directly. In the example
above, after encountering the cell at coordinates 3/0, the cell at 1/2
must be skipped and must either be jumped-to later, or reached from 3/1,
itself reachable from 3/0 in one of the single-step moves available to
Achilles. The Stack manipulated by Achilles is used to represent this
skipping and later accessing of cells. In general, skipped cells are
left dangling isolated from the rest of the object until the Input is
empty, at which point, if any cells remain on the Stack, Achilles tries
to draw a continuous line through as many of the cells on the Stack as
possible. Cells that can't be accessed from other cells on the Stack are
jumped-to directly.

Below, we ask Achilles to trace a plan through the example above, of the
cross-shaped object (the query to trace_plan/2 follows from where the
query in the previous example ended):

==
?- image(cross,_Is),image_scan(_Is,_Ss),objects(_Ss,_Os),member(_Cs,_Os), foreground_object(_Cs), portray_object(_Cs), trace_plan(_Cs,_Ps), nl, print_clauses(_Ps), length(_Ps,_N), format('Plan steps: ~d~n~n', [_N]).
[cell(1,7-5,3/0),cell(1,7-5,3/1),cell(1,7-5,1/2),cell(1,7-5,2/2),cell(1,7-5,3/2),cell(1,7-5,4/2),cell(1,7-5,5/2),cell(1,7-5,3/3),cell(1,7-5,3/4)].
. . . b . . .
. . . b . . .
. b b b b b .
. . . b . . .
. . . b . . .

start(cell(1,7-5,3/0)).
move_south(cell(1,7-5,3/0),cell(1,7-5,3/1)).
move_south_west(cell(1,7-5,3/1),cell(1,7-5,2/2)).
move_east(cell(1,7-5,2/2),cell(1,7-5,3/2)).
move_east(cell(1,7-5,3/2),cell(1,7-5,4/2)).
move_east(cell(1,7-5,4/2),cell(1,7-5,5/2)).
goto(cell(1,7-5,1/2)).
goto(cell(1,7-5,3/3)).
move_south(cell(1,7-5,3/3),cell(1,7-5,3/4)).
stop(cell(1,7-5,3/4)).
Plan steps: 10

true ;
false.
==

Note the path that Achilles chooses - after moving south to the cell at
coordinates 3/1 (from the first cell in the input, 3/0), Achilles skips
1/2 and continues with the cell at 2/2, which is reachable from 3/1 with
a south-easterly move. Achilles then continues with a straight line
eastwards, that ends at 5/2, from which no other cells in the input are
reachable (only the cells at 3/3 and 3/4 remain in the Input while the
Stack holds 1/2). Therefore, Achilles pops the cell at 1/2 from the
Stack and jumps directly to it. Then, it pushes the cells at 3/3 and 3/4
to the Stack, pops the cell at 3/3 and jumps directly to it. Finally, it
moves south from 3/3 to 3/4 which it can reach with a simple move.

Note that the push and pop operations on the stack, as well as the read
operations that take the next cell from the Input are not represented in
the plan. The idea is to generate a plan that any agent that has access
to the same primitive moves as Achilles (including a jump to an
arbitrary location) can follow. Since there's no guarantee that such an
agent will also have a concept of a Stack, or an Input, Achilles only
returns a plan that consists of primitive moves passing through all
cells in an object, while crossing as many adjacent cells as possible.

*/

%!      direction(?Direction) is semidet.
%
%       One of the eight directions: N, NE, E, etc.
%
direction(D):-
        member(D,[east
                 ,south_east
                 ,south
                 ,south_west
                 ,west
                 ,north_west
                 ,north
                 ,north_east
               ]).


%!      trace_plan(+Cells,-Plan) is det.
%
%       Find a Plan to draw a list of Cells.
%
trace_plan([C],[start(C)|Is]):-
% No need for complex plans!
        !
        ,go_to(t(C,east,down),C,[stop(C)],Is).
trace_plan(Cs,Ps):-
        start(Cs,T1,Cs_,Acc)
        ,trace_plan(T1,Cs_,[],Acc,Ps_)
        ,reverse(Ps_,Ps).

%!      trace_plan(+Cursor,+Cells,+Stack,+Acc,-Trace) is det.
%
%       Business end of trace_plan/2
%
%       Cursor is a single cell, cell(C,W-H,X/Y), marking the current
%       position of the (imaginary) tracing cursor.
%
%       Cells is the list of cells remaining in the shape being traced.
%
%       Stack is an initially empty list onto which we can push cells
%       that cannot be reached from the current position without lifting
%       the pen up, so they need to be stored and restored later.
%
%       Trace is a list of movement instructions that can be used to
%       restore the traced shape, or in other words, a drawing plan for
%       the shape.
%
%       Movement instructions are primitive moves like move_south,
%       move_east, etc, that move the tracing cursor a single cell to
%       the named direction. A go_to instruction is also available, that
%       moves the cursor to an arbitrary cell, taken from the Stack.
%
%       This predicate traces a path from the start of a shape, at the
%       call closest to the top-left corner of an image, to the end of
%       the shape at the cell closest to the bottom-right corner of an
%       image.
%
%       Cells are given as a list of contiguous, but not necessarily
%       adjacent cells, therefore it is not always possible to draw a
%       shape in one go, without skipping some cells and jumping to a
%       new location. To represent this, trace_shape/5 implements the
%       following rule:
%
%       a) If the next cell in Cells is adjacent to Cursor move to the
%       next cell in Cells.
%
%       b) Else, if the next cell in Stack is adjacent to Cursor move
%       to the next cell in Stack.
%
%       d) Else, push the next cell in Cells onto Stack and continue
%       with the rest of Cells.
%
%       In other words: if we can move to the next cell in Cells with a
%       primitive move, we move to that cell (and remove it from the
%       input list of Cells). Otherwise first we try popping a cell from
%       the Stack. And if that fails, we push the current input cell and
%       continue with the current Cursor.
%
%       The process exits when the input list of Cells and the Stack
%       are both empty (meaning we have walked over every Cell
%       initially in the input).
%
trace_plan(T,Cs,Ss,Is,[stop(T)|Is]):-
% Nothing more to draw.
        empty_input(Cs)
        ,empty_stack(Ss)
        ,!.

trace_plan(T1,Cs,Ss,Acc1,Bind):-
% WRITE from input
        read(Cs,C,Cs_,Acc1,Acc2)
        ,move_to(T1,C,Acc2,Acc3)
        ,!
        ,trace_plan(C,Cs_,Ss,Acc3,Bind).

trace_plan(T1,Cs,Ss,Acc1,Bind):-
% POP and WRITE from stack
        pop(Ss,C,Ss_,Acc1,Acc2)
        ,move_to(T1,C,Acc2,Acc3)
        ,!
        ,trace_plan(C,Cs,Ss_,Acc3,Bind).

trace_plan(T1,Cs,Ss,Acc1,Bind):-
% POP from stack and JUMP
        pop(Ss,C,Ss_,Acc1,Acc2)
        ,go_to(T1,C,Acc2,Acc3)
        ,!
        ,trace_plan(C,Cs,Ss_,Acc3,Bind).

trace_plan(T,Cs,Ss,Acc1,Bind):-
% PUSH to stack
        read(Cs,C,Cs_,Acc1,Acc2)
        ,push(Ss,C,Ss_,Acc2,Acc3)
        ,trace_plan(T,Cs_,Ss_,Acc3,Bind).


%!      start(+Cells,-Cursor,-Rest,-Instructions) is det.
%
%       Starting state of the trace.
%
start([C1|Cs],C1,Cs,[start(C1)]).


%!      move_to(+Cursor,+New,+Acc,-Instructions) is det.
%
%       Move the Cursor to a New cell.
%
move_to(C1,C2,Is,[M|Is]):-
        adjacent(C1,C2,H2)
        ,atom_concat(move_,H2,D)
        ,M =.. [D,C1,C2].


%!      adjacent(+Cell1,+Cell2,?Direction) is det.
%
%       True when Cell1 is next to Cell2 in that Direction.
%
%       Deterministic wrapper around adjacent_/3. We only need to find
%       an adjacent cell in one direction - no need to go looking any
%       further after that.
%
adjacent(C1,C2,D):-
        once(adjacent_(C1,C2,D)).

%!      adjacent_(+Cell1,+Cell2,?Direction) is nondet.
%
%       Business end of adjacent/3.
%
adjacent_(C1,C2,D):-
        direction(D)
        ,M =.. [D,C1,C2]
        ,call(vision_thing:M).


%!      go_to(+Cursor,-New,+Acc,-Instructions) is det.
%
%       Jump from the Cursor to a New cell.
%
go_to(_C1,C2,Is,[goto(C2)|Is]).


%!      read(+Cells,-Cell,-Rest,+Acc,-Instructions) is det.
%
%       Read a new Cell from the input.
%
read([C|Cs],C,Cs,Is,Is).
%read([C|Cs],C,Cs,Is,[read(C)|Is]).


%!      pop(+Stack,-Cell,-Rest,+Acc,-Instructions) is det.
%
%       Pop a Cell from the Stack.
%
pop([C|Ss],C,Ss,Is,Is).
%pop([C|Ss],C,Ss,Is,[pop(C)|Is]).


%!      push(+Stack,+Cell,-New,+Acc,-Instructions) is det.
%
%       Push a Cell onto the Stack.
%
push(Ss,C,[C|Ss],Is,Is).
%push(Ss,C,[C|Ss],Is,[push(C)|Is]).


%!      empty_input(+Cells) is det.
%
%       True when the input list of Cells is empty.
%
%       Silly silly thingy.
%
empty_input([]).


%!      empty_stack(+Stack) is det.
%
%       True when the Stack is empty.
%
%       Silly silly thingy number 2.
%
empty_stack([]).
