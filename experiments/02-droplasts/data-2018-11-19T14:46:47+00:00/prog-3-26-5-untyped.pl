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
my_reverse1(A,B):-reverse(A,B).
my_pred2(A,B):-succ(B,A).
my_tail3([_|TL],TL).
my_sumlist4(A,B):-sumlist(A,B).
my_tail5([_|TL],TL).
my_last6(A,B):-last(A,B).
my_tail7([_|TL],TL).
my_tail8([_|TL],TL).
my_pred9(A,B):-succ(B,A).
my_len10(A,B):-length(A,B).
my_reverse11(A,B):-reverse(A,B).
my_min_list12(A,B):-min_list(A,B).
my_reverse13(A,B):-reverse(A,B).
my_last14(A,B):-last(A,B).
my_max_list15(A,B):-max_list(A,B).
my_len16(A,B):-length(A,B).
my_len17(A,B):-length(A,B).
my_min_list18(A,B):-min_list(A,B).
my_pred19(A,B):-succ(B,A).
my_max_list20(A,B):-max_list(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_head22([H|_],H).
my_head23([H|_],H).
my_reverse24(A,B):-reverse(A,B).
my_min_list25(A,B):-min_list(A,B).
prim(my_succ0/2).
prim(my_reverse1/2).
prim(my_pred2/2).
prim(my_tail3/2).
prim(my_sumlist4/2).
prim(my_tail5/2).
prim(my_last6/2).
prim(my_tail7/2).
prim(my_tail8/2).
prim(my_pred9/2).
prim(my_len10/2).
prim(my_reverse11/2).
prim(my_min_list12/2).
prim(my_reverse13/2).
prim(my_last14/2).
prim(my_max_list15/2).
prim(my_len16/2).
prim(my_len17/2).
prim(my_min_list18/2).
prim(my_pred19/2).
prim(my_max_list20/2).
prim(my_sumlist21/2).
prim(my_head22/2).
prim(my_head23/2).
prim(my_reverse24/2).
prim(my_min_list25/2).
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
p([['b','v','j','b'],['a','k','x'],['m','c','i'],['d','r','m','x']],[['b','v','j'],['a','k'],['m','c'],['d','r','m']]).
p([['w','d','o','r'],['t','q','g','x'],['i','h','c']],[['w','d','o'],['t','q','g'],['i','h']]).
