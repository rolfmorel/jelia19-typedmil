:- use_module('metagol').
:- use_module(library(system)).
metagol:max_clauses(3).

metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
my_succ1(X,Y):-Y is X+1,Y<5000.
my_succ2(X,Y):-Y is X+1,Y<5000.
my_succ3(X,Y):-Y is X+1,Y<5000.
my_succ4(X,Y):-Y is X+1,Y<5000.
my_succ5(X,Y):-Y is X+1,Y<5000.
my_succ6(X,Y):-Y is X+1,Y<5000.
my_succ7(X,Y):-Y is X+1,Y<5000.
my_succ8(X,Y):-Y is X+1,Y<5000.
my_succ9(X,Y):-Y is X+1,Y<5000.
my_succ10(X,Y):-Y is X+1,Y<5000.
my_succ11(X,Y):-Y is X+1,Y<5000.
my_succ12(X,Y):-Y is X+1,Y<5000.
my_succ13(X,Y):-Y is X+1,Y<5000.
my_succ14(X,Y):-Y is X+1,Y<5000.
sucker15(X,Y):-Y is X+1,Y<5000.
sucker16(X,Y):-Y is X+1,Y<5000.
sucker17(X,Y):-Y is X+1,Y<5000.
sucker18(X,Y):-Y is X+1,Y<5000.
sucker19(X,Y):-Y is X+1,Y<5000.
sucker20(X,Y):-Y is X+1,Y<5000.
prim(my_succ1/2).
prim(my_succ2/2).
prim(my_succ3/2).
prim(my_succ4/2).
prim(my_succ5/2).
prim(my_succ6/2).
prim(my_succ7/2).
prim(my_succ8/2).
prim(my_succ9/2).
prim(my_succ10/2).
prim(my_succ11/2).
prim(my_succ12/2).
prim(my_succ13/2).
prim(my_succ14/2).
prim(sucker15/2).
prim(sucker16/2).
prim(sucker17/2).
prim(sucker18/2).
prim(sucker19/2).
prim(sucker20/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  catch(call_with_time_limit(MaxTime, (learn([p(1,0)],[],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  format('%prog,~w\n',[H]),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,num_well_typed,14\n"),
  format("%data,total_preds,20\n"),
  format("%data,types_enabled,False\n").
