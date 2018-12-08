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
my_len0(A,B):-length(A,B).
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_reverse3(A,B):-reverse(A,B).
my_len4(A,B):-length(A,B).
my_head5([H|_],H).
my_sumlist6(A,B):-sumlist(A,B).
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A).
my_reverse9(A,B):-reverse(A,B).
my_head10([H|_],H).
my_pred11(A,B):-succ(B,A).
my_sumlist12(A,B):-sumlist(A,B).
my_min_list13(A,B):-min_list(A,B).
my_last14(A,B):-last(A,B).
my_last15(A,B):-last(A,B).
my_min_list16(A,B):-min_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
prim(my_len0/2).
prim(my_min_list1/2).
prim(my_succ2/2).
prim(my_reverse3/2).
prim(my_len4/2).
prim(my_head5/2).
prim(my_sumlist6/2).
prim(my_last7/2).
prim(my_pred8/2).
prim(my_reverse9/2).
prim(my_head10/2).
prim(my_pred11/2).
prim(my_sumlist12/2).
prim(my_min_list13/2).
prim(my_last14/2).
prim(my_last15/2).
prim(my_min_list16/2).
prim(my_sumlist17/2).
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
p([['b','s','g','t'],['b','n','d','q'],['y','t','u','q'],['s','m','i']],[['b','s','g'],['b','n','d'],['y','t','u'],['s','m']]).
p([['y','g','v'],['l','u','b','x'],['s','x','l']],[['y','g'],['l','u','b'],['s','x']]).
