:-module(image_data, [image/2
                     ]).

/** <module> Training data for vision_thing dataset.
*/

%!      image(-Image) is nondet.
%
%       An Image in ARC format.
%
%       Just some test images to test things.
%
image(S,E):-
        memberchk(S, [cross
                     ,square
                     ,line
                     ,rectangle
                     ,point
                     ])
        ,I =.. [S,E]
        ,call(I).


%!      cross(-Cross) is nondet.
%
%       Examples of images containing crosses.
%
cross(E):-
        member(E, [[[0,1,0]
                   ,[1,1,1]
                   ,[0,1,0]
                   ]
                  ,[[0,0,0,1,0,0,0]
                   ,[0,0,0,1,0,0,0]
                   ,[0,1,1,1,1,1,0]
                   ,[0,0,0,1,0,0,0]
                   ,[0,0,0,1,0,0,0]
                   ]
                  ]).

%!      square(-Square) is nondet.
%
%       Examples of images containing squares.
%
square(E):-
        member(E, [[[1,1,0]
                   ,[1,1,0]
                   ,[0,0,0]
                   ]
                  ,[[0,1,1]
                   ,[0,1,1]
                   ,[0,0,0]
                   ]
                  ,[[0,0,0]
                   ,[0,1,1]
                   ,[0,1,1]
                   ]
                  ,[[0,0,0]
                   ,[1,1,0]
                   ,[1,1,0]
                   ]
                  ]).

%!      line(-Line) is nondet.
%
%       Examples of images containing lines.
%
line(E):-
        member(E, [[[0,1,0]
                   ,[0,1,0]
                   ,[0,1,0]
                   ]
                  ,[[1,1,1]
                   ,[0,0,0]
                   ,[1,1,1]
                   ]
                  ,[[0,1,0]
                   ,[0,1,0]
                   ,[0,1,0]
                   ,[0,2,0]
                   ,[0,2,0]
                   ,[0,2,0]
                   ]
                  ,[[1,1,1,2,2,2]
                   ,[0,0,0,0,0,0]
                   ]
                  ]).

%!      rectangle(-Rectangle) is nondet.
%
%       Examples of images containing rectangles.
%
rectangle(E):-
        member(E, [[[1,1,1,0,0]
                   ,[1,1,1,0,0]
                   ,[0,3,3,3,3]
                   ,[0,3,3,3,3]
                   ]
                  ]).

%!      rectangle(-Rectangle) is nondet.
%
%       Examples of images containing rectangles.
%
point(E):-
        member(E, [[[1,0,2,0,0]
                   ,[0,3,0,4,0]
                   ,[0,0,5,0,6]
                   ,[7,0,8,0,9]
                   ]
                  ]).
