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
my_last1(A,B):-last(A,B).
my_pred2(A,B):-succ(B,A).
my_tail3([_|TL],TL).
my_last4(A,B):-last(A,B).
my_tail5([_|TL],TL).
my_head6([H|_],H).
my_last7(A,B):-last(A,B).
my_head8([H|_],H).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_len11(A,B):-length(A,B).
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_tail14([_|TL],TL).
my_pred15(A,B):-succ(B,A).
my_max_list16(A,B):-max_list(A,B).
my_reverse17(A,B):-reverse(A,B).
my_head18([H|_],H).
my_succ19(A,B):-succ(A,B).
my_max_list20(A,B):-max_list(A,B).
my_max_list21(A,B):-max_list(A,B).
my_tail22([_|TL],TL).
my_head23([H|_],H).
my_max_list24(A,B):-max_list(A,B).
prim(my_succ0/2).
prim(my_last1/2).
prim(my_pred2/2).
prim(my_tail3/2).
prim(my_last4/2).
prim(my_tail5/2).
prim(my_head6/2).
prim(my_last7/2).
prim(my_head8/2).
prim(my_min_list9/2).
prim(my_reverse10/2).
prim(my_len11/2).
prim(my_head12/2).
prim(my_len13/2).
prim(my_tail14/2).
prim(my_pred15/2).
prim(my_max_list16/2).
prim(my_reverse17/2).
prim(my_head18/2).
prim(my_succ19/2).
prim(my_max_list20/2).
prim(my_max_list21/2).
prim(my_tail22/2).
prim(my_head23/2).
prim(my_max_list24/2).
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
p([['c','k','s','o'],['i','i','m'],['b','h','r','k']],[['c','k','s'],['i','i'],['b','h','r']]).
p([['o','c','r','j'],['o','o','p'],['q','d','o']],[['o','c','r'],['o','o'],['q','d']]).
