:- use_module('metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).

tail([_|T],T).

prim(tail/2).
prim(reverse/2).

map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
my_succ0(A,B):-succ(A,B).
my_len1(A,B):-length(A,B).
my_pred2(A,B):-succ(B,A).
my_reverse3(A,B):-reverse(A,B).
my_succ4(A,B):-succ(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B).
my_tail7([_|TL],TL).
my_pred8(A,B):-succ(B,A).
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A).
my_sumlist11(A,B):-sumlist(A,B).
my_max_list12(A,B):-max_list(A,B).
my_tail13([_|TL],TL).
my_last14(A,B):-last(A,B).
my_max_list15(A,B):-max_list(A,B).
my_min_list16(A,B):-min_list(A,B).
my_min_list17(A,B):-min_list(A,B).
my_pred18(A,B):-succ(B,A).
prim(my_succ0/2).
prim(my_len1/2).
prim(my_pred2/2).
prim(my_reverse3/2).
prim(my_succ4/2).
prim(my_sumlist5/2).
prim(my_succ6/2).
prim(my_tail7/2).
prim(my_pred8/2).
prim(my_last9/2).
prim(my_pred10/2).
prim(my_sumlist11/2).
prim(my_max_list12/2).
prim(my_tail13/2).
prim(my_last14/2).
prim(my_max_list15/2).
prim(my_min_list16/2).
prim(my_min_list17/2).
prim(my_pred18/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  catch(call_with_time_limit(MaxTime, (learn(Pos,[],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p([['y','g','t'],['l','b','h','x'],['v','i','t','m']],[['y','g'],['l','b','h'],['v','i','t']]).
p([['o','o','y'],['b','n','e']],[['o','o'],['b','n']]).
