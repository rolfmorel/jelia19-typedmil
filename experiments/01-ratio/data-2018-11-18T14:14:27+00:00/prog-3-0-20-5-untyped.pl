:- use_module('metagol').
:- use_module(library(system)).
metagol:max_clauses(3).

metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
sucker1(X,Y):-Y is X+1,Y<5000.
sucker2(X,Y):-Y is X+1,Y<5000.
sucker3(X,Y):-Y is X+1,Y<5000.
sucker4(X,Y):-Y is X+1,Y<5000.
sucker5(X,Y):-Y is X+1,Y<5000.
sucker6(X,Y):-Y is X+1,Y<5000.
sucker7(X,Y):-Y is X+1,Y<5000.
sucker8(X,Y):-Y is X+1,Y<5000.
sucker9(X,Y):-Y is X+1,Y<5000.
sucker10(X,Y):-Y is X+1,Y<5000.
sucker11(X,Y):-Y is X+1,Y<5000.
sucker12(X,Y):-Y is X+1,Y<5000.
sucker13(X,Y):-Y is X+1,Y<5000.
sucker14(X,Y):-Y is X+1,Y<5000.
sucker15(X,Y):-Y is X+1,Y<5000.
sucker16(X,Y):-Y is X+1,Y<5000.
sucker17(X,Y):-Y is X+1,Y<5000.
sucker18(X,Y):-Y is X+1,Y<5000.
sucker19(X,Y):-Y is X+1,Y<5000.
sucker20(X,Y):-Y is X+1,Y<5000.
prim(sucker1/2).
prim(sucker2/2).
prim(sucker3/2).
prim(sucker4/2).
prim(sucker5/2).
prim(sucker6/2).
prim(sucker7/2).
prim(sucker8/2).
prim(sucker9/2).
prim(sucker10/2).
prim(sucker11/2).
prim(sucker12/2).
prim(sucker13/2).
prim(sucker14/2).
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
  format("%data,num_well_typed,0\n"),
  format("%data,total_preds,20\n"),
  format("%data,types_enabled,False\n").
