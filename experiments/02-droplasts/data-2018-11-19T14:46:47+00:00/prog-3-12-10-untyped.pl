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
my_pred0(A,B):-succ(B,A).
my_max_list1(A,B):-max_list(A,B).
my_reverse2(A,B):-reverse(A,B).
my_reverse3(A,B):-reverse(A,B).
my_min_list4(A,B):-min_list(A,B).
my_succ5(A,B):-succ(A,B).
my_tail6([_|TL],TL).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B).
my_pred9(A,B):-succ(B,A).
my_sumlist10(A,B):-sumlist(A,B).
my_min_list11(A,B):-min_list(A,B).
prim(my_pred0/2).
prim(my_max_list1/2).
prim(my_reverse2/2).
prim(my_reverse3/2).
prim(my_min_list4/2).
prim(my_succ5/2).
prim(my_tail6/2).
prim(my_reverse7/2).
prim(my_succ8/2).
prim(my_pred9/2).
prim(my_sumlist10/2).
prim(my_min_list11/2).
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
p([['q','w','q','b'],['g','c','o','q'],['b','a','n']],[['q','w','q'],['g','c','o'],['b','a']]).
p([['u','m','b'],['r','b','c'],['x','u','u']],[['u','m'],['r','b'],['x','u']]).
