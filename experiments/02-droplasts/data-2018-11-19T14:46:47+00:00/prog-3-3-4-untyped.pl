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
my_max_list2(A,B):-max_list(A,B).
prim(my_pred0/2).
prim(my_max_list1/2).
prim(my_max_list2/2).
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
p([['r','n','y','j'],['p','a','n'],['i','i','p','a'],['r','a','p']],[['r','n','y'],['p','a'],['i','i','p'],['r','a']]).
p([['t','d','f'],['i','u','g','g'],['y','x','h'],['o','d','c']],[['t','d'],['i','u','g'],['y','x'],['o','d']]).
