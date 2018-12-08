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
my_head0([H|_],H).
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_head3([H|_],H).
my_min_list4(A,B):-min_list(A,B).
my_succ5(A,B):-succ(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_succ7(A,B):-succ(A,B).
my_pred8(A,B):-succ(B,A).
prim(my_head0/2).
prim(my_min_list1/2).
prim(my_succ2/2).
prim(my_head3/2).
prim(my_min_list4/2).
prim(my_succ5/2).
prim(my_sumlist6/2).
prim(my_succ7/2).
prim(my_pred8/2).
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
p([['a','x','i','k'],['c','j','b','b'],['c','l','s'],['s','r','d','c']],[['a','x','i'],['c','j','b'],['c','l'],['s','r','d']]).
p([['l','f','i'],['p','o','t'],['p','t','u','s']],[['l','f'],['p','o'],['p','t','u']]).
