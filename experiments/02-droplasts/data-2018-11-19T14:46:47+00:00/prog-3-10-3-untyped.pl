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
my_sumlist1(A,B):-sumlist(A,B).
my_succ2(A,B):-succ(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_pred6(A,B):-succ(B,A).
my_succ7(A,B):-succ(A,B).
my_len8(A,B):-length(A,B).
my_tail9([_|TL],TL).
prim(my_head0/2).
prim(my_sumlist1/2).
prim(my_succ2/2).
prim(my_sumlist3/2).
prim(my_max_list4/2).
prim(my_head5/2).
prim(my_pred6/2).
prim(my_succ7/2).
prim(my_len8/2).
prim(my_tail9/2).
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
p([['k','q','v'],['r','j','g','d'],['o','v','b','j'],['d','q','e']],[['k','q'],['r','j','g'],['o','v','b'],['d','q']]).
p([['s','a','d','n'],['e','h','u'],['r','c','u','x']],[['s','a','d'],['e','h'],['r','c','u']]).
