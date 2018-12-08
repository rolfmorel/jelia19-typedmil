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
my_sumlist2(A,B):-sumlist(A,B).
my_succ3(A,B):-succ(A,B).
my_pred4(A,B):-succ(B,A).
my_len5(A,B):-length(A,B).
prim(my_pred0/2).
prim(my_max_list1/2).
prim(my_sumlist2/2).
prim(my_succ3/2).
prim(my_pred4/2).
prim(my_len5/2).
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
p([['c','x','u'],['i','j','d','v'],['m','r','w','w']],[['c','x'],['i','j','d'],['m','r','w']]).
p([['n','v','e'],['s','g','k'],['t','v','v','y'],['i','w','p']],[['n','v'],['s','g'],['t','v','v'],['i','w']]).
