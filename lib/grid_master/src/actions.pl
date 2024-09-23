:-module(actions, [step_up/3
                  ,step_right/3
                  ,step_down/3
                  ,step_left/3
                  ]).

:-use_module(grid_master_src(action_generator)).

/** <module> Action definitions for action generator.

*/


%!      step_up(+Representation,+Start,-Step) is det.
%
%       Generate a Step up from a Start location.
%
%       Representation is the current value of grid_master_configuration
%       option action_representation/1: one of [stack_less, stack_based,
%       lookaround, controller_sequences].
%
%       Start is a list [Map,Coordinates] where Map is a map/3 term and
%       Coordinates is a pair X/Y, the coordinates of the start location
%       of the move to be generated. See the start of the file for more
%       on map/3 terms.
%
%       Step is an atom of the step_up/2 primitive move action. The form
%       of Step depends on the value of action_representation/1.
%
%       step_up/2 generates steps with a rotation applied to coordinates
%       to transform them into Cartesian coordinates, with the origin
%       at the lower-left corner.
%
step_up(stack_based,[map(Id,Dims,Ms),X/Y]
       ,step_up([Id,X/Y,T,[T|Os],[up|As]],[Id,X_/Y_,T_,Os,As])):-
        step(X/Y,+,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').
step_up(stack_less,[map(Id,Dims,Ms),X/Y],step_up([Id,X/Y,T],[Id,X_/Y_,T_])):-
        step(X/Y,+,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true).
step_up(lookaround,[map(Id,Dims,Ms),X/Y]
       ,step_up([Id,X/Y,T,[O|Os],[up|As]],[Id,X_/Y_,T_,Os,As])):-
        step(X/Y,+,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,look_around(X/Y,Ms,Dims,O)
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').
step_up(controller_sequences,[map(Id,Dims,Ms),X/Y]
       ,step_up([Id,X/Y,T,Q0,[Q0|Qs],[O|Os],[up|As],[q0|Qs_]]
               ,[Id,X_/Y_,T_,q0,Qs,Os,As,Qs_])):-
        step(X/Y,+,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,look_around(X/Y,Ms,Dims,O)
        ,Q0 = '$VAR'('Q0')
        ,Qs = '$VAR'('Qs')
        ,Os = '$VAR'('Os')
        ,As = '$VAR'('As')
        ,Qs_ = '$VAR'('Qs_').
step_up(list_based,[map(Id,Dims,Ms),X/Y],step_up([Id,X/Y,T,Vs],[Id,X_/Y_,T_,[up|Vs]])):-
        step(X/Y,+,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,Vs = '$VAR'('Vs').


%!      step_right(+Representation,+Start,-Step) is det.
%
%       Generate a Step tot he right of a Start location.
%
%       As step_up/2 but moves right.
%
step_right(stack_based,[map(Id,Dims,Ms),X/Y]
          ,step_right([Id,X/Y,T,[T|Os],[right|As]],[Id,X_/Y_,T_,Os,As])):-
        step(X/Y,+,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').
step_right(stack_less,[map(Id,Dims,Ms),X/Y],step_right([Id,X/Y,T],[Id,X_/Y_,T_])):-
        step(X/Y,+,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true).
step_right(lookaround,[map(Id,Dims,Ms),X/Y]
       ,step_right([Id,X/Y,T,[O|Os],[right|As]],[Id,X_/Y_,T_,Os,As])):-
        step(X/Y,+,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,look_around(X/Y,Ms,Dims,O)
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').
step_right(controller_sequences,[map(Id,Dims,Ms),X/Y]
       ,step_right([Id,X/Y,T,Q0,[Q0|Qs],[O|Os],[right|As],[q1|Qs_]]
                  ,[Id,X_/Y_,T_,q1,Qs,Os,As,Qs_])):-
        step(X/Y,+,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,look_around(X/Y,Ms,Dims,O)
        ,Q0 = '$VAR'('Q0')
        ,Qs = '$VAR'('Qs')
        ,Os = '$VAR'('Os')
        ,As = '$VAR'('As')
        ,Qs_ = '$VAR'('Qs_').
step_right(list_based,[map(Id,Dims,Ms),X/Y]
          ,step_right([Id,X/Y,T,Vs],[Id,X_/Y_,T_,[right|Vs]])):-
        step(X/Y,+,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,Vs = '$VAR'('Vs').


%!      step_down(+Representation,+Start,-Step) is det.
%
%       Generate a Step down from a Start location.
%
%       As step_up/2 but moves down.
%
step_down(stack_based,[map(Id,Dims,Ms),X/Y]
         ,step_down([Id,X/Y,T,[T|Os],[down|As]],[Id,X_/Y_,T_,Os,As])):-
        step(X/Y,-,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').
step_down(stack_less,[map(Id,Dims,Ms),X/Y],step_down([Id,X/Y,T],[Id,X_/Y_,T_])):-
        step(X/Y,-,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true).
step_down(lookaround,[map(Id,Dims,Ms),X/Y]
       ,step_down([Id,X/Y,T,[O|Os],[down|As]],[Id,X_/Y_,T_,Os,As])):-
        step(X/Y,-,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,look_around(X/Y,Ms,Dims,O)
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').
step_down(controller_sequences,[map(Id,Dims,Ms),X/Y]
         ,step_down([Id,X/Y,T,Q0,[Q0|Qs],[O|Os],[down|As],[q2|Qs_]]
                   ,[Id,X_/Y_,T_,q2,Qs,Os,As,Qs_])):-
        step(X/Y,-,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,look_around(X/Y,Ms,Dims,O)
        ,Q0 = '$VAR'('Q0')
        ,Qs = '$VAR'('Qs')
        ,Os = '$VAR'('Os')
        ,As = '$VAR'('As')
        ,Qs_ = '$VAR'('Qs_').
step_down(list_based,[map(Id,Dims,Ms),X/Y]
         ,step_down([Id,X/Y,T,Vs],[Id,X_/Y_,T_,[down|Vs]])):-
        step(X/Y,-,0/1,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,Vs = '$VAR'('Vs').


%!      step_left(+Representation,+Start,-Step) is det.
%
%       Generate a Step to the left of a Start location.
%
%       As step_up/2 but moves left.
%
step_left(stack_based,[map(Id,Dims,Ms),X/Y]
         ,step_left([Id,X/Y,T,[T|Os],[left|As]],[Id,X_/Y_,T_,Os,As])):-
        step(X/Y,-,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').
step_left(stack_less,[map(Id,Dims,Ms),X/Y],step_left([Id,X/Y,T],[Id,X_/Y_,T_])):-
        step(X/Y,-,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true).
step_left(lookaround,[map(Id,Dims,Ms),X/Y]
       ,step_left([Id,X/Y,T,[O|Os],[left|As]],[Id,X_/Y_,T_,Os,As])):-
        step(X/Y,-,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,look_around(X/Y,Ms,Dims,O)
        ,As = '$VAR'('As')
        ,Os = '$VAR'('Os').
step_left(controller_sequences,[map(Id,Dims,Ms),X/Y]
       ,step_left([Id,X/Y,T,Q0,[Q0|Qs],[O|Os],[left|As],[q3|Qs_]]
                 ,[Id,X_/Y_,T_,q3,Qs,Os,As,Qs_])):-
        step(X/Y,-,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,look_around(X/Y,Ms,Dims,O)
        ,Q0 = '$VAR'('Q0')
        ,Qs = '$VAR'('Qs')
        ,Os = '$VAR'('Os')
        ,As = '$VAR'('As')
        ,Qs_ = '$VAR'('Qs_').
step_left(list_based,[map(Id,Dims,Ms),X/Y]
         ,step_left([Id,X/Y,T,Vs],[Id,X_/Y_,T_,[left|Vs]])):-
        step(X/Y,-,1/0,Ms,Dims,X_/Y_)
        ,map_location(X/Y,T,Ms,Dims,true)
        ,map_location(X_/Y_,T_,Ms,Dims,true)
        ,Vs = '$VAR'('Vs').
