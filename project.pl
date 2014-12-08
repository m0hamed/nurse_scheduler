% filetype=prolog

:- use_module(library(clpfd)).

% how long the schedule is
scheduling_days(5).

% main predicate
solve(Schedule):-
  Num_nurses in 20..100,
  length(Schedule, Num_nurses),
  nurse_constrains(Schedule),
  day_constrains(Schedule),
  flatten(Schedule, Vars),
  labeling([ff], Vars),
  true.

% all constraints for a single nurse should be listed here
nurse_constrains([]).
nurse_constrains([L|T]):-
  scheduling_days(Days),
  length(L, Days),
  L ins 0..3,
  non_successive_shifts(L),
  working_hours(L),
  max_day_offs(L),
  min_day_offs(L),
  no_bridge_days(L),
  nurse_constrains(T).

day_constrains(L):-
  scheduling_days(Days),
  num_nurses(L, Days),
  true.

num_nurses(_, 0).
num_nurses(L, DayID):-
  maplist(element(DayID), L, Day),
  global_cardinality(Day, [1-Morning, 2-Evening, 3-Night, 0-_]),
  Morning #>= 3,
  Evening #>= 3,
  Night #>= 2,
  D2 is DayID-1,
  num_nurses(L, D2).

% requires at least 2 shifts off
non_successive_shifts([_]).
non_successive_shifts([H1, H2|T]):-
  H1 #= 0 #\/ (H2 #< H1 #==> H2 #= 0),
  non_successive_shifts([H2|T]).

% flatten the schedule for labeling
flatten([],[]).
flatten([H|T], R):-
  append(H, R2, R),
  flatten(T, R2).

working_hours(L):-
  global_cardinality(L, [1-A, 2-B, 3-C, 0-_]),
  [A, B, C] ins 0..14,
  C #=< 4,
  A+B+C #=< 10,
  true.

min_day_offs([_,_,_,_,_]).
min_day_offs([A,B,C,D,E,F|T]):-
  global_cardinality([A,B,C,D,E,F], [1-_, 2-_, 3-_, 0-DayOffs]),
  DayOffs #>= 1,
  min_day_offs([B,C,D,E,F|T]),
  true.

max_day_offs([_,_,_,_]).
max_day_offs([A,B,C,D,E|T]):-
  global_cardinality([A,B,C,D,E], [1-_, 2-_, 3-_, 0-DayOffs]),
  DayOffs #=< 2,
  min_day_offs([B,C,D,E|T]),
  true.

no_bridge_days([_,_]).
no_bridge_days([H1,H2,H3|T]):-
  (H1#= 0 #/\ H2#\= 0) #==> H3#\=0,
  no_bridge_days([H2,H3|T]).

