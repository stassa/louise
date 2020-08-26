:-module(rendering,[render_examples/1
                   ,portray_object/1
                   ,portray_object/2
                   ,portray_object/3
                   ]).

:-use_module(image).
:-use_module(rendering_config).

/** <module> Render two-dimensional images.

Predicates in this module are used to visualise objects in ARC format
images, by printing them out to the console in glorious ASCII.

Examples of use:

==
?- image(line,_Is), image_scan(_Is,_Ss), portray_object('Image',_Ss), objects(_Ss,_Os), member(_Cs,_Os), foreground_object(_Cs), nl, portray_object('Foreground',_Cs), ordered_object(_Cs,_Rs), nl, portray_object('Ordered',_Rs).
Image
[cell(0,3-3,0/0),cell(1,3-3,1/0),cell(0,3-3,2/0),cell(0,3-3,0/1),cell(1,3-3,1/1),cell(0,3-3,2/1),cell(0,3-3,0/2),cell(1,3-3,1/2),cell(0,3-3,2/2)].
# b #
# b #
# b #

Foreground
[cell(1,3-3,1/0),cell(1,3-3,1/1),cell(1,3-3,1/2)].
. b .
. b .
. b .

Ordered
[cell(1,3-3,1/0),cell(1,3-3,1/1),cell(1,3-3,1/2)].
. b .
. b .
. b .
true .
==


*/

%!      render_examples(+Examples:list) is det.
%
%       Render a list of Examples for an ARC task.
%
render_examples(Es):-
        forall(member(P,Es)
              ,(render_instance(P)
               ,nl
               )
              ).


%!      render_instance(+Instance:list) is det.
%
%       Pretty-print an Input or Output instance in a pair.
%
render_instance(Ts):-
        forall(member(Rs,Ts)
              ,(forall(member(C,Rs)
                      ,render_tile(C)
                      )
               ,nl)
              ).


%!      render_tile(+Tile:atom) is det.
%
%       Pretty-print a tile in a task grid.
%
%       A "tile" is my terminology for the square elements in a grid.
%       In the ARC dataset, each tile is represented as a one-digit
%       number denoting colour. render_tile/1 maps these numbers to
%       characters denoting colour. See colour_symbol/2 for the mapping.
%
render_tile(C):-
        colour_symbol(C,S)
        ,format('~w ', [S]).


%!      portray_object(+Message,+Object) is det.
%
%       Render an object on an empty background.
%
%       As portray_object/1 but also prints an informative Message.
%
portray_object(Ms,[cell(C,W-H,X/Y)|Cs]):-
        writeln(Ms)
        ,portray_object(W,H,[cell(C,W-H,X/Y)|Cs]).


%!      portray_object(+Object) is det.
%
%       Render an object on an empty background.
%
%       Use this to visualise an object in order to debug object
%       detection etc.
%
%       The dimensions of the empty grid on which the Object is drawn
%       are tekan from the width and height information in the cell/3
%       terms of the Object (i.e. their second argument).
%
portray_object([cell(C,W-H,X/Y)|Cs]):-
        portray_object(W,H,[cell(C,W-H,X/Y)|Cs]).


%!      portray_object(+W,+H,+Object) is det.
%
%       Render an object on an empty background.
%
%       As portrary_object/1 but the dimensions of the background grid
%       are specified in the input arguments W and H, allowing objects
%       to be drawn in larger (or smaller!) grids than those of the
%       images from which they were originally extracted.
%
portray_object(W,H,Cs):-
        W_ is W - 1
        ,H_ is H - 1
        ,print_clauses([Cs])
        ,portray_object(0,0,W_,H_,Cs).

%!      portray_object(+X,+Y,+W,+H,+Object) is det.
%
%       Business end of portray_object/3.
%
portray_object(W,H,W,H,Cs):-
% Cs_ should be [] but just in case it isn't
% we ignore it or we'll go IIiiiIIIinfinite.
        !
       ,render_tile(W/H,W-H,Cs,_)
       ,nl.
portray_object(W,Y,W,H,Cs):-
        !
        ,render_tile(W/Y,W-H,Cs,Cs_)
        ,succ(Y,Y_)
        ,nl
        ,portray_object(0,Y_,W,H,Cs_).
portray_object(X,Y,W,H,Cs):-
        !
        ,render_tile(X/Y,W-H,Cs,Cs_)
        ,succ(X,X_)
        ,portray_object(X_,Y,W,H,Cs_).


%!      render_tile(+X,+Y,+Cells,-Remaining) is det.
%
%       Render a tile with the appropriate colour.
%
render_tile(X1/Y1,W-H,[cell(_C,_D,X/Y)|Cs],Cs_):-
% Ignore cells outside the image bounds but render the rest of the image
% normally.
        (\+ within_limits(X,W)
        ; \+ within_limits(Y,H)
        )
        ,render_tile(X1/Y1,W-H,Cs,Cs_)
        ,!.
render_tile(X/Y,_Dim,[cell(0,_D,X/Y)|Cs],Cs):-
        !
        ,render_tile(-1).
render_tile(X/Y,_Dim,[cell(C,_D,X/Y)|Cs],Cs):-
        !
        ,render_tile(C).
render_tile(_X/_Y,_Dim,Cs,Cs):-
        render_tile(0).

