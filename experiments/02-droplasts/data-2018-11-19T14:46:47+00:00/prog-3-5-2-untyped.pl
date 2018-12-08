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
my_reverse0(A,B):-reverse(A,B).
my_reverse1(A,B):-reverse(A,B).
my_reverse2(A,B):-reverse(A,B).
my_pred3(A,B):-succ(B,A).
my_last4(A,B):-last(A,B).
prim(my_reverse0/2).
prim(my_reverse1/2).
prim(my_reverse2/2).
prim(my_pred3/2).
prim(my_last4/2).
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
p([['t','o','a','g'],['b','o','v'],['a','d','u','s']],[['t','o','a'],['b','o'],['a','d','u']]).
p([['e','f','g'],['v','r','x'],['f','w','s','m'],['y','u','b','j']],[['e','f'],['v','r'],['f','w','s'],['y','u','b']]).
