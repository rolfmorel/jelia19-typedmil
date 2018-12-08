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
my_tail2([_|TL],TL).
my_succ3(A,B):-succ(A,B).
my_tail4([_|TL],TL).
my_len5(A,B):-length(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_pred7(A,B):-succ(B,A).
my_head8([H|_],H).
my_head9([H|_],H).
my_sumlist10(A,B):-sumlist(A,B).
my_min_list11(A,B):-min_list(A,B).
my_pred12(A,B):-succ(B,A).
my_reverse13(A,B):-reverse(A,B).
my_tail14([_|TL],TL).
my_min_list15(A,B):-min_list(A,B).
my_pred16(A,B):-succ(B,A).
my_sumlist17(A,B):-sumlist(A,B).
my_pred18(A,B):-succ(B,A).
prim(my_tail0/2).
prim(my_max_list1/2).
prim(my_tail2/2).
prim(my_succ3/2).
prim(my_tail4/2).
prim(my_len5/2).
prim(my_sumlist6/2).
prim(my_pred7/2).
prim(my_head8/2).
prim(my_head9/2).
prim(my_sumlist10/2).
prim(my_min_list11/2).
prim(my_pred12/2).
prim(my_reverse13/2).
prim(my_tail14/2).
prim(my_min_list15/2).
prim(my_pred16/2).
prim(my_sumlist17/2).
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
p([['o','o','p'],['q','l','k'],['l','r','l'],['o','w','r','s']],[['o','o'],['q','l'],['l','r'],['o','w','r']]).
p([['b','x','y'],['g','x','o','m'],['l','i','b','q'],['h','f','e']],[['b','x'],['g','x','o'],['l','i','b'],['h','f']]).
