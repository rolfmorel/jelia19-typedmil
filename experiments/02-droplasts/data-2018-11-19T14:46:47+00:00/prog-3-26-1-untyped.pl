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
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_pred3(A,B):-succ(B,A).
my_pred4(A,B):-succ(B,A).
my_reverse5(A,B):-reverse(A,B).
my_max_list6(A,B):-max_list(A,B).
my_tail7([_|TL],TL).
my_last8(A,B):-last(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_max_list11(A,B):-max_list(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_head13([H|_],H).
my_sumlist14(A,B):-sumlist(A,B).
my_succ15(A,B):-succ(A,B).
my_min_list16(A,B):-min_list(A,B).
my_head17([H|_],H).
my_last18(A,B):-last(A,B).
my_last19(A,B):-last(A,B).
my_tail20([_|TL],TL).
my_pred21(A,B):-succ(B,A).
my_head22([H|_],H).
my_pred23(A,B):-succ(B,A).
my_reverse24(A,B):-reverse(A,B).
my_succ25(A,B):-succ(A,B).
prim(my_succ0/2).
prim(my_min_list1/2).
prim(my_succ2/2).
prim(my_pred3/2).
prim(my_pred4/2).
prim(my_reverse5/2).
prim(my_max_list6/2).
prim(my_tail7/2).
prim(my_last8/2).
prim(my_last9/2).
prim(my_head10/2).
prim(my_max_list11/2).
prim(my_sumlist12/2).
prim(my_head13/2).
prim(my_sumlist14/2).
prim(my_succ15/2).
prim(my_min_list16/2).
prim(my_head17/2).
prim(my_last18/2).
prim(my_last19/2).
prim(my_tail20/2).
prim(my_pred21/2).
prim(my_head22/2).
prim(my_pred23/2).
prim(my_reverse24/2).
prim(my_succ25/2).
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
p([['d','v','v'],['q','q','p'],['n','d','s','x'],['w','i','s','u']],[['d','v'],['q','q'],['n','d','s'],['w','i','s']]).
p([['j','s','v','s'],['x','y','o','x']],[['j','s','v'],['x','y','o']]).
