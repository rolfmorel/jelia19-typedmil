:- use_module('metagol-typed').
:- use_module(library(system)).
metagol:max_clauses(3).

metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
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
prim(sucker1,[bottom,bottom]).
prim(sucker2,[bottom,bottom]).
prim(sucker3,[bottom,bottom]).
prim(sucker4,[bottom,bottom]).
prim(sucker5,[bottom,bottom]).
prim(sucker6,[bottom,bottom]).
prim(sucker7,[bottom,bottom]).
prim(sucker8,[bottom,bottom]).
prim(sucker9,[bottom,bottom]).
prim(sucker10,[bottom,bottom]).
prim(sucker11,[bottom,bottom]).
prim(sucker12,[bottom,bottom]).
prim(sucker13,[bottom,bottom]).
prim(sucker14,[bottom,bottom]).
prim(sucker15,[bottom,bottom]).
prim(sucker16,[bottom,bottom]).
prim(sucker17,[bottom,bottom]).
prim(sucker18,[bottom,bottom]).
prim(sucker19,[bottom,bottom]).
prim(sucker20,[bottom,bottom]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  catch(call_with_time_limit(MaxTime, (learntyped([p(1,0)],[],[int,int],H);true)),
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
  format("%data,types_enabled,True\n").
