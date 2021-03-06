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
my_last0(A,B):-last(A,B).
my_pred1(A,B):-succ(B,A).
my_last2(A,B):-last(A,B).
my_pred3(A,B):-succ(B,A).
my_sumlist4(A,B):-sumlist(A,B).
my_min_list5(A,B):-min_list(A,B).
prim(my_last0/2).
prim(my_pred1/2).
prim(my_last2/2).
prim(my_pred3/2).
prim(my_sumlist4/2).
prim(my_min_list5/2).
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
p([['i','r','m'],['p','n','i']],[['i','r'],['p','n']]).
p([['n','y','r'],['q','w','y','x'],['h','g','c','v']],[['n','y'],['q','w','y'],['h','g','c']]).
