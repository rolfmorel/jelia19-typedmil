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
my_pred1(A,B):-succ(B,A).
my_max_list2(A,B):-max_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_reverse5(A,B):-reverse(A,B).
my_last6(A,B):-last(A,B).
my_pred7(A,B):-succ(B,A).
my_reverse8(A,B):-reverse(A,B).
my_succ9(A,B):-succ(A,B).
my_succ10(A,B):-succ(A,B).
my_last11(A,B):-last(A,B).
my_succ12(A,B):-succ(A,B).
my_succ13(A,B):-succ(A,B).
my_tail14([_|TL],TL).
my_sumlist15(A,B):-sumlist(A,B).
my_succ16(A,B):-succ(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_last18(A,B):-last(A,B).
my_succ19(A,B):-succ(A,B).
my_min_list20(A,B):-min_list(A,B).
my_max_list21(A,B):-max_list(A,B).
my_min_list22(A,B):-min_list(A,B).
my_reverse23(A,B):-reverse(A,B).
my_pred24(A,B):-succ(B,A).
my_len25(A,B):-length(A,B).
my_tail26([_|TL],TL).
my_len27(A,B):-length(A,B).
my_len28(A,B):-length(A,B).
my_tail29([_|TL],TL).
prim(my_len0/2).
prim(my_pred1/2).
prim(my_max_list2/2).
prim(my_max_list3/2).
prim(my_sumlist4/2).
prim(my_reverse5/2).
prim(my_last6/2).
prim(my_pred7/2).
prim(my_reverse8/2).
prim(my_succ9/2).
prim(my_succ10/2).
prim(my_last11/2).
prim(my_succ12/2).
prim(my_succ13/2).
prim(my_tail14/2).
prim(my_sumlist15/2).
prim(my_succ16/2).
prim(my_sumlist17/2).
prim(my_last18/2).
prim(my_succ19/2).
prim(my_min_list20/2).
prim(my_max_list21/2).
prim(my_min_list22/2).
prim(my_reverse23/2).
prim(my_pred24/2).
prim(my_len25/2).
prim(my_tail26/2).
prim(my_len27/2).
prim(my_len28/2).
prim(my_tail29/2).
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
p([['r','w','m'],['g','n','x','a']],[['r','w'],['g','n','x']]).
p([['m','g','p','f'],['y','d','p'],['s','m','c','x']],[['m','g','p'],['y','d'],['s','m','c']]).
