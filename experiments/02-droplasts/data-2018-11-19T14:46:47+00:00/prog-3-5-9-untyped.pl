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
my_min_list0(A,B):-min_list(A,B).
my_sumlist1(A,B):-sumlist(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_pred4(A,B):-succ(B,A).
prim(my_min_list0/2).
prim(my_sumlist1/2).
prim(my_sumlist2/2).
prim(my_sumlist3/2).
prim(my_pred4/2).
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
p([['i','o','l','t'],['g','v','e','s'],['w','d','g'],['o','i','v']],[['i','o','l'],['g','v','e'],['w','d'],['o','i']]).
p([['l','g','x','b'],['q','c','s','m']],[['l','g','x'],['q','c','s']]).
