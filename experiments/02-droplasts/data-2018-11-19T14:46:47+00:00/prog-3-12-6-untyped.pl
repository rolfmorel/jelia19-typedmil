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
my_pred1(A,B):-succ(B,A).
my_head2([H|_],H).
my_max_list3(A,B):-max_list(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_max_list5(A,B):-max_list(A,B).
my_max_list6(A,B):-max_list(A,B).
my_len7(A,B):-length(A,B).
my_tail8([_|TL],TL).
my_pred9(A,B):-succ(B,A).
my_min_list10(A,B):-min_list(A,B).
my_head11([H|_],H).
prim(my_pred0/2).
prim(my_pred1/2).
prim(my_head2/2).
prim(my_max_list3/2).
prim(my_sumlist4/2).
prim(my_max_list5/2).
prim(my_max_list6/2).
prim(my_len7/2).
prim(my_tail8/2).
prim(my_pred9/2).
prim(my_min_list10/2).
prim(my_head11/2).
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
p([['h','g','m','r'],['n','n','r','q'],['n','q','t']],[['h','g','m'],['n','n','r'],['n','q']]).
p([['b','b','o'],['e','e','l'],['e','r','t','m']],[['b','b'],['e','e'],['e','r','t']]).
