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
                        % Primitive moves
                       ,move_north/2
                       ,move_north_east/2
                       ,move_east/2
                       ,move_south_east/2
                       ,move_south/2
                       ,move_south_west/2
                       ,move_west/2
                       ,move_north_west/2
                       ,stay/2
                       ,move/4
                       ]).

:-user:use_module(rendering).
:-user:use_module(image).
:-user:use_module(image_data).

/** <module> Object identification in an ARC dataset image.

*/

:- auxiliaries:set_configuration_option(max_invented, [3]).

configuration:metarule_constraints(M,fail):-
% Avoid starting with stop/2. Because that's just dumb.
        M =.. [m,_Id,_P,Q|_Ps]
        ,Q = stop.

configuration:metarule_constraints(M,fail):-
% Only stop once per clause. I mean, jeez, Louise.
        M =.. [m,_Id,_P|Ps]
        ,findall(1
                ,(member(stop,Ps))
                ,Ss)
        ,sumlist(Ss,N)
        ,N > 1.


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

metarules(_/2,[chain]).

positive_example(S/2,E):-
        image(S,Is)
        ,image_scan(Is,Cs)
        ,objects(Cs,Os)
        ,member(O, Os)
        ,foreground_object(O)
        ,E =.. [S,O,[]].

negative_example(shape/2,_):-
        fail.

go_north, [C2] --> [C1,C2], { move_north(C1,C2) }.
go_north_east, [C2] --> [C1,C2], { move_north_east(C1,C2) }.
go_east, [C2] --> [C1,C2], { move_east(C1,C2) }.
go_south_east, [C2] --> [C1,C2], { move_south_east(C1,C2) }.
go_south, [C2] --> [C1,C2], { move_south(C1,C2) }.
go_south_west, [C2] --> [C1,C2], { move_south_west(C1,C2) }.
go_west, [C2] --> [C1,C2], { move_west(C1,C2) }.
go_north_west, [C2] --> [C1,C2], { move_north_west(C1,C2) }.
stop --> [C], { stay(C,C) }.


%!      move_north(+Cell1,-Cell2) is det.
%
%       Move towards the image north.
%
move_north(cell(C,P1),cell(C,P2)):-
        move(P1,-,0/1,P2).

%!      move_north_east(+Cell1,-Cell2) is det.
%
%       Move towards the image north-east.
%
move_north_east(cell(C,P1),cell(C,P2)):-
        move(P1,+,1/0,P_)
        ,move(P_,-,0/1,P2).

%!      move_east(+Cell1,-Cell2) is det.
%
%       Move towards the image east.
%
move_east(cell(C,P1),cell(C,P2)):-
        move(P1,+,1/0,P2).

%!      move_south_east(+Cell1,-Cell2) is det.
%
%       Move towards the image south-east.
%
move_south_east(cell(C,P1),cell(C,P2)):-
        move(P1,+,1/0,P_)
        ,move(P_,+,0/1,P2).

%!      move_south(+Cell1,-Cell2) is det.
%
%       Move towards the image south.
%
move_south(cell(C,P1),cell(C,P2)):-
        move(P1,+,0/1,P2).

%!      move_south_west(+Cell1,-Cell2) is det.
%
%       Move towards the image south-west.
%
move_south_west(cell(C,P1),cell(C,P2)):-
        move(P1,-,1/0,P_)
        ,move(P_,+,0/1,P2).

%!      move_west(+Cell1,-Cell2) is det.
%
%       Move towards the image west.
%
move_west(cell(C,P1),cell(C,P2)):-
        move(P1,-,1/0,P2).

%!      move_north_west(+Cell1,-Cell2) is det.
%
%       Move towards the image north-west.
%
move_north_west(cell(C,P1),cell(C,P2)):-
        move(P1,-,1/0,P_)
        ,move(P_,-,0/1,P2).

%!      stay(+Cell1,-Cell2) is det.
%
%       Stay put.
%
stay(C,C).


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
