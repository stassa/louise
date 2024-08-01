:-module(controller_freak_configuration, [controller/3
                                         ,debug_map/2
                                         ,debug_environment/3
                                         ,environment/2
                                         ,executor/1
                                         ,mark_slamming_map/1
                                         ,overlay_precedence/2
                                         ,output_actions/1
                                         ,table_executor/1
                                         ]).

/** <module> Configuration options for Controller Freak.

*/

:-dynamic(executor/1).


%!      controller(?Path,?Module,?Symbol) is det.
%
%       Name and path of a controller's Module and its Symbol.
%
%       Used to find a controller for executor/5.
%
controller(controllers/'grid_navigation_controller.pl',grid_navigation_controller,t).


%!      debug_map(?Subject,?How) is semidet.
%
%       Whether to debug a Subject, and how.
%
%       Determines whether map_logging/2 prints a map for "logging" or
%       not.
%
%       Subject is a debug subject received from map_logging/2.
%
%       How is one of: [tiles, coordinates, both, false]. "false" can be
%       substituted for any term that is not 'tiles', 'coordinates' or
%       'both', e.g. 'none' etc. Any term that is not recognised by
%       print_map/2 and friends.
%
%       @tbd Should definitely be in grid_master but can't be arsed
%       right now.
%
debug_map(_,false).
%debug_map(mark,tiles).
%debug_map(slam_update,tiles).


%!      debug_environment(?Subject,?How,?Time) is semidet.
%
%       Whether to debug a Subject or not, and How.
%
%       Determines whether environments being solved will be logged to
%       the screen, or not. This is different to logging a map, which
%       only prints a map; logging the "environment" also logs the
%       agent's path through the map.
%
%       How is one of: [tiles, coordinates, both, false]. "false" can be
%       substituted for any term that is not 'tiles', 'coordinates' or
%       'both', e.g. 'none' etc. Any term that is not recognised by
%       print_map/2 and friends.
%
%       Time is a boolean denoting whether the environment display will
%       be timed.
%
%debug_environment(none,_,_).
debug_environment(search,tiles,true).
debug_environment(seek,tiles,true).
debug_environment(solve,tiles,true).


%!      environment(?Path,?Module) is semidet.
%
%       Name and path to a Module that defines the current environment.
%
%       The "current environment" is a set of rules that simulate an
%       environment, and the results of an agent's actions in that
%       environment.
%
environment(lib/grid_master/data/environments/basic_environment/'basic_environment.pl'
           ,basic_environment).


%!      executor(?Executor) is semidet.
%
%       Executor loop to use with controllers.
%
%       Known Executor values:
%       * backtracking: magickally teleport back in time.
%       * reversing: painstakingly reverse course.
%
%executor(reversing).
%executor(backtracking).
executor(reversing_slam).
%executor(backtracking_slam).


%!      mark_slamming_map(?Bool) is semidet.
%
%       Whether to map the position of an agent on a SLAM map, or not.
%
%       See overlay_precedence/2 for options controlling how the map is
%       marked.
%
mark_slamming_map(true).


%!      overlay_precedence(?Case,?Precedence) is semidet.
%
%       Precedence terms when overlaying a map on top of another.
%
%       Case is an identifier for the clause of this option to be used
%       by predicates that call map overlay predicates.
%
%       Precedence is a list of key-value pairs T1 > T2 where T1 and T2
%       are tile symbols. Precedence may be empty and controls how tiles
%       are written from Overlaying on top of tiles in Underlying. If a
%       term T1 > T2 exists in Precedence, then a tile T1 can be written
%       over a tile T2. Otherwise, T1 cannot be written on top of T2.
%       This doesn't cause the overlaying operation to fail - only to
%       ignore the attempt to write T1 on top of T2 unless T1 > T2 is
%       there.
%
% a) Search a map for an exit tile [seeking].
% b) Search, then seek [survey].
overlay_precedence(extended_map,[]).
overlay_precedence(grid_slam,[s>_,'@'>f,'@'>x,w>x,f>x,e>x]).
overlay_precedence(mark,[s>_,e>_]). % Mark exit tiles.
overlay_precedence(survey_mission,[s>_,e>_,'@'>f,'@'>x,w>x,f>x]).

% Search a map exhaustively and return to start [searching]
%overlay_precedence(extended_map,[]).
%overlay_precedence(grid_slam,[s>_,'@'>f,'@'>x,w>x,f>x,e>x]).
%overlay_precedence(mark,[s>_,'@'>e]). % Don't mark exit tiles.
%overlay_precedence(survey_mission,[s>_,e>_,'@'>f,'@'>x,w>x,f>x]).

% Patrol between landmarks, marking start and exit tile [patrol]
%overlay_precedence(extended_map,[]).
% Allow marking landmarks as exit tiles:
%overlay_precedence(grid_slam,[s>_,'@'>f,'@'>x,'@'>e,e>x,e>f,w>x,f>x]).
%overlay_precedence(mark,[s>_,e>_]). % Mark exit tiles.
%overlay_precedence(mark,[s>_,'@'>e]). % Don't mark exit tiles.
%overlay_precedence(survey_mission,[s>_,e>_,'@'>f,'@'>x,w>x,f>x]).


%!      output_actions(?Form) is semidet.
%
%       Form of actions output by executor/6.
%
%       Form is one of: [labels, tuples, executor_states]. Those are
%       interpreted as follows:
%
%       * labels: each element of the list of actions output by
%         executor/6 is an atomic action label found in a controller
%         tuple.
%       * tuples: each element of the actions list is a controller tuple
%         T(Q0,O,A,Q1)
%       * executor_states: each element of the actions list is a pair
%         S:T where S is an executor state, one of: [trv, rev, alt], and
%         T is a controller tuple T(Q0,O,A,Q1), as for 'tuple'.
%
%       Form 'executor_states' will fail unless the current executor is
%       a reversing executor, i.e. if the value of executor/1 is a
%       reference to a reversing executor.
%
%       @tbd More to the point, this option is currently only called by
%       reversing executors.
%
output_actions(labels).
%output_actions(tuples).
%output_actions(executor_states).


%!      table_executor(?Bool) is semidet.
%
%       Whether to table the current executor, or not.
%
%       @tbd Setting bool to true causes backtracking executors to
%       display very weird behaviour, e.g. they completely miss their
%       target and exit anyway. Needs some TLC.
%
table_executor(false).
%table_executor(true).
