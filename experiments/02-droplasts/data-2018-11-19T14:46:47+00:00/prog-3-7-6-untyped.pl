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
my_sumlist0(A,B):-sumlist(A,B).
my_succ1(A,B):-succ(A,B).
my_succ2(A,B):-succ(A,B).
my_last3(A,B):-last(A,B).
my_max_list4(A,B):-max_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_last6(A,B):-last(A,B).
prim(my_sumlist0/2).
prim(my_succ1/2).
prim(my_succ2/2).
prim(my_last3/2).
prim(my_max_list4/2).
prim(my_min_list5/2).
prim(my_last6/2).
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
p([['t','a','n'],['w','k','s','k'],['w','u','m','g']],[['t','a'],['w','k','s'],['w','u','m']]).
p([['p','v','j'],['u','c','m','c']],[['p','v'],['u','c','m']]).
