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
my_succ1(A,B):-succ(A,B).
my_tail2([_|TL],TL).
my_pred3(A,B):-succ(B,A).
my_min_list4(A,B):-min_list(A,B).
my_pred5(A,B):-succ(B,A).
my_last6(A,B):-last(A,B).
my_head7([H|_],H).
my_min_list8(A,B):-min_list(A,B).
my_min_list9(A,B):-min_list(A,B).
prim(my_last0/2).
prim(my_succ1/2).
prim(my_tail2/2).
prim(my_pred3/2).
prim(my_min_list4/2).
prim(my_pred5/2).
prim(my_last6/2).
prim(my_head7/2).
prim(my_min_list8/2).
prim(my_min_list9/2).
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
p([['y','f','c','o'],['d','h','o'],['m','r','b']],[['y','f','c'],['d','h'],['m','r']]).
p([['d','c','j','e'],['x','x','t'],['s','p','h'],['g','j','d']],[['d','c','j'],['x','x'],['s','p'],['g','j']]).
