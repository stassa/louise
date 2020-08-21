:-module(image, [image_dimensions/3
                ,image_scan/2
                ,objects/2
                ,within_limits/2
                ,foreground_object/1
                ,ordered_object/2
                ]).

/** <module> Represntation of images in vision_thing dataset.

We expect an "image" to be represented as a grid of cells each with a
distinct colour, as for the ARC dataset (rather than a matrix of pixel
values).

We represent an ARC "image" as a list of lists, where each sublist is a
row in the image grid and each element of a sublist is a cell in the
row represented by the sublist. For example, the following Prolog list
is an ARC image with a single blue line of two-cells length in the
middle:

==
Is = [[0,0,0]
     ,[0,1,0]
     ,[0,1,0]
     ]
==

In keeping with image processing tradition we assume that the image's
coordinates have their origine in the upper left corner, as in an
inverted cartesian system. For example, those are the coordinates of
the image above:

==
Is = [[0/0,1/0,2/0]
     ,[0/1,1/1,2/1]
     ,[0/2,1/2,2/2]
     ]
==

And of any 3 x 3 image of course. To confuse matters further and pour
epater les bourjeois, we count each dimension of an image starting at 1
rather than 0, so a 3 x 3 image starts at the upper left corner in the
0/0 coordinate and ends in the lower right corner in the 2/2 coordinate.

*/


%!      image_dimensions(+Image,-Width,-Height) is det.
%
%       True when Width, Height are the dimensions of Image.
%
%       Note that image dimensions are counted from 1, unlike the origin
%       of the coordinates in an image that start at 0/0. Thus, an image
%       with W = 3 and H = 3 starts at the upper left corner in 0/0 and
%       ends at the lower right corner in 2/2.
%
image_dimensions([Cs|Rs],W,H):-
        length(Cs,W)
        ,length([Cs|Rs],H).


%!      image_scan(+Image,-Cells) is det.
%
%       Scan an image from left to right and top to bottom.
%
%       Image is a list of lists each representing a row in an ARC
%       image, where each sub-list is a row and each element of a
%       sub-list is the colour of a cell in that row.
%
%       Cells is a list of compounds cell(C,X/Y) representing cells,
%       where C is an integer from 0-9 representing the colour of the
%       cell and X/Y is a coordinate in the image.
%
image_scan(Rs,Cs):-
        image_dimensions(Rs,W,H)
        ,image_scan(Rs,W-H,0/0,[],Cs).

%!      image_scan(+Rows,+Dimensions,+Current,+Acc,-Cells) is det.
%
%       Business end of image_scan/2.
%
%       Current is the current cell in the position of the scanning
%       "head" in the image.
%
image_scan([[]],_D,_P,Cs,Cs):-
        !.
image_scan(Rs,D,P,Acc,[cell(C,P)|Bind]):-
        right_scan([Rs,D,P,C],[Rs_,D,P_,_C_])
        ,image_scan(Rs_,D,P_,Acc,Bind).


%!      right_scan(+State1,-State2) is det.
%
%       Scan right from the current image State.
%
right_scan([Rs,W-H,X/Y,C],[[Cs|Rs_],W-H,New,C_]):-
        right_scan(Rs,Cs,Rs_,C,C_)
        ,!
        ,move(X/Y,+,1/0,W-H,New).


%!      right_scan(+Image,-Row,-Next_Row,-Cell,-Next_Cell) is semidet.
%
%       Scan right in the current Row.
%
%       Image is a list of lists representing the "pixels" of an image,
%       where each "pixel" is a single numeric value denoting colour.
%
%       Cell is the pixel in the image on which the read-head is
%       currently resting.
%
%       Next_Cell is the pixel adjacent to Cell and to its right.
%
%       Row is the list of pixels remaining in thecurrent row being
%       scanned after Next_Cell.
%
%       Next_Row is the next row after Row, in Image.
%
right_scan([[C,C_|Cs]|Rs],[C_|Cs],Rs,C,C_).
right_scan([[C]|[[C_|Cs]|Rs]],[C_|Cs],Rs,C,C_).
right_scan([[C]],[],[],C,C).


%!	move(+Point,+Delta,+Distance,+Limits,-End) is det.
%
%	Modify a coordinate, respecting spatial Limits.
%
%	Point is a compound X/Y wher X,Y are numbers, representing a
%	coordinate. Delta is one of [-,+], signifying how Point is to be
%	modified. Distance is a compound, Dx/Dy, where Dx,Dy are
%	numbers, each the amount by which the corresponding value in
%	Point is to be modified according to Delta. Limits is a
%	key-value pair, W-H, where each of W, H are numbers, the upper
%	limit of the two dimensions of the current world.
%
%	move/5 is true when End = Point Delta Distance and End_X in
%	[0,W], End_Y in [0,H].
%
%       @tbd Copied from data/robots/move_generator.pl and updated to
%       allow wrapping back to the start of a new row when the maximum
%       width coordinate is exceeded.
%
move(X/Y,D,Dx/Dy,W-H,Ex/Ey):-
        succ(X,W)
        ,!
        ,succ(Y,Y_)
        ,Dx_ is Dx - 1
        ,move(0/Y_,D,Dx_/Dy,W-H,Ex/Ey).
move(X/Y,D,Dx/Dy,W-H,Ex/Ey):-
	ground(X/Y)
	,ground(Dx/Dy)
	,ground(D)
	,ground(W-H)
	,Mv_x =.. [D,X,Dx]
	,Mv_y =.. [D,Y,Dy]
	,Ex is Mv_x
	,Ey is Mv_y
	,within_limits(Ex,W)
	,within_limits(Ey,H).


%!	within_limits(+Distance,+Limit) is det.
%
%	True when a moving Distance is within the given Limit.
%
within_limits(X,L):-
	integer(X)
	,integer(L)
	,X >= 0
	,X =< L.


%!      objects(+Image,-Objects) is det.
%
%       Detect all Objects in an Image.
%
%       By "object" we mean a set of cells that a) have the same colour
%       and b) are contiguous, i.e. have a Euclidean distance more than
%       0 and less than 2. Contiguousness is a transitive relation so if
%       C1 is contiguous with C2 and C2 contiguous with C3, C1 is
%       contiguous with C3.
%
objects(Cs,Os):-
        objects(Cs,[],Os).

%!      objects(+Image,+Acc,-Objects) is det.
%
%       Business end of objects/2.
%
objects([],Os,Os).
objects([C|Cs],Acc,[Os|Bind]):-
        contiguous(C,Cs,Os,Ds)
        ,!
        ,objects(Ds,Acc,Bind).
objects([_|Cs],Acc,Bind):-
        objects(Cs,Acc,Bind).


%!      contiguous(+Cell,+Image,-Contiguous,-Discontiguous) is det.
%
%       Collect cells Contiguous to a Cell in an Image.
%
%       Cell is a compound cell(C,X/Y) where C is the colour and X/Y the
%       coordinates of a cell in an Image. Image is a list of cells.
%       Contiguous is the list of cells in Image contiguous to Cell and
%       Discontiguous the list of remaining cells in the Image.
%
%       Two cells are considered to be contiguous iff they have the same
%       colour and their X/Y dimensions have a Euclidean distance
%       between 1 and less than 2.
%
%       The unstated assumption is that Cell is the first cell in a
%       shape, meaning the left- and upper-most cell in a set of
%       contiguous cells forming the shape.
%
contiguous(C,Cs,[C|Os],Ds):-
        contiguous(C,Cs,[],Ds_,[],Os_)
        ,ordered_object(Os_,Os)
        ,ordered_object(Ds_,Ds).

%!      contiguous(+Cell,+Imag,+Acc1,-Cont,+Acc2,-Discont) is det.
%
%       Business end of contiguous/4.
%
contiguous(_C,[],Ds,Ds,Os,Os):-
        !.
contiguous(C1,[C1|Cs],Ds_Acc,Ds_Bind,Acc,Bind):-
        !
        ,contiguous(C1,Cs,Ds_Acc,Ds_Bind,Acc,Bind).
contiguous(C1,[C2|Cs],Ds_Acc,Ds_Bind,Acc,Bind):-
        contiguous_(C1,C2)
        ,!
        ,contiguous(C1,Cs,Ds_Acc,Ds_Acc1,[C2|Acc],Acc2)
        ,contiguous(C2,Ds_Acc1,[],Ds_Bind,Acc2,Bind).
contiguous(C1,[C2|Cs],Ds_Acc,Ds_Bind,Acc,Bind):-
        contiguous(C1,Cs,[C2|Ds_Acc],Ds_Bind,Acc,Bind).


%!      contiguous_(+Cell1,+Cell2) is det.
%
%       True when Cell1 and Cell2 are part of the same shape.
%
contiguous_(C1,C2):-
        same_colour(C1,C2)
        ,adjacent_cell(C1,C2).


%!      same_colour(+Cell1,Cell2) is det.
%
%       True when Cell1 and Cell2 have the same colour.
%
same_colour(cell(C,_),cell(C,_)).


%!      adjacent_cell(+Cell1,+Cell2) is det.
%
%       True when Cell1 is adjacent to Cell2.
%
adjacent_cell(cell(_,P1),cell(_,P2)):-
        adjacent(P1,P2).


%!      adjacent(+Cell1,+Cell2) is det.
%
%       True when Cell is adjacent to Cell2 in an image.
%
%       Two cells are adjacent when their Euclidean distance D is
%       in [1.0, 2.0), or 1 =< D < 2.
%
adjacent(C1,C2):-
        euclidean_distance(C1,C2,D)
        ,1.0 =< D
        ,D < 2.0.


%!      euclidean_distance(+Cell1,Cell2,-Distance) is det.
%
%       Quick and dirty Euclidean Distance of two cells.
%
euclidean_distance(X1/Y1, X2/Y2, D):-
        X is X2-X1
        ,Y is Y2-Y1
        ,D is sqrt(X^2 + Y^2).



%!      foreground_object(+Object) is det.
%
%       True when Object has no cells of the background colour.
%
foreground_object(Os):-
        findall(1
               ,member(cell(0,_),Os)
               ,[]).


%!      ordered_object(+Object,-Reordered) is det.
%
%       Order an object in the starndard object order.
%
%       Object is a list of image cells, cell(C,X/Y), where C a colour
%       and X/Y the x and y axis coordinates of the cell in an image
%       greid.
%
%       Reordered is the list of cells in Object reordered by the
%       standard order of objects in this project. The standard order
%       for objects is ascending on the y axis then ascending on the x
%       axis. For example, the following is a list of object cells
%       ordered in the standard order for objects:%
%       ==
%       cell(1,3/0)
%       cell(1,3/1)
%       cell(1,1/2)
%       cell(1,2/2)
%       cell(1,3/2)
%       cell(1,4/2)
%       cell(1,5/2)
%       cell(1,3/3)
%       cell(1,3/4)
%       ==
%
ordered_object(Os,Os_):-
        findall(cell(C,X/Y)
               ,order_by([asc(Y)
                         ,asc(X)]
                        ,member(cell(C,X/Y),Os)
                        )
               ,Os_).
