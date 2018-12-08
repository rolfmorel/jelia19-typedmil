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
my_tail0([_|TL],TL).
my_max_list1(A,B):-max_list(A,B).
my_max_list2(A,B):-max_list(A,B).
my_last3(A,B):-last(A,B).
my_succ4(A,B):-succ(A,B).
my_tail5([_|TL],TL).
my_succ6(A,B):-succ(A,B).
my_succ7(A,B):-succ(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_len10(A,B):-length(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_head13([H|_],H).
my_last14(A,B):-last(A,B).
my_head15([H|_],H).
my_succ16(A,B):-succ(A,B).
my_tail17([_|TL],TL).
my_last18(A,B):-last(A,B).
my_head19([H|_],H).
my_min_list20(A,B):-min_list(A,B).
my_pred21(A,B):-succ(B,A).
my_pred22(A,B):-succ(B,A).
my_len23(A,B):-length(A,B).
my_pred24(A,B):-succ(B,A).
my_sumlist25(A,B):-sumlist(A,B).
my_succ26(A,B):-succ(A,B).
my_max_list27(A,B):-max_list(A,B).
my_min_list28(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_max_list1/2).
prim(my_max_list2/2).
prim(my_last3/2).
prim(my_succ4/2).
prim(my_tail5/2).
prim(my_succ6/2).
prim(my_succ7/2).
prim(my_sumlist8/2).
prim(my_sumlist9/2).
prim(my_len10/2).
prim(my_sumlist11/2).
prim(my_sumlist12/2).
prim(my_head13/2).
prim(my_last14/2).
prim(my_head15/2).
prim(my_succ16/2).
prim(my_tail17/2).
prim(my_last18/2).
prim(my_head19/2).
prim(my_min_list20/2).
prim(my_pred21/2).
prim(my_pred22/2).
prim(my_len23/2).
prim(my_pred24/2).
prim(my_sumlist25/2).
prim(my_succ26/2).
prim(my_max_list27/2).
prim(my_min_list28/2).
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
p([['m','d','i'],['c','r','w','h']],[['m','d'],['c','r','w']]).
p([['w','g','i'],['h','m','r','j'],['k','v','m','d'],['o','r','m']],[['w','g'],['h','m','r'],['k','v','m'],['o','r']]).
