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
my_max_list1(A,B):-max_list(A,B).
my_tail2([_|TL],TL).
my_last3(A,B):-last(A,B).
my_len4(A,B):-length(A,B).
my_reverse5(A,B):-reverse(A,B).
my_tail6([_|TL],TL).
my_pred7(A,B):-succ(B,A).
my_last8(A,B):-last(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_tail11([_|TL],TL).
prim(my_last0/2).
prim(my_max_list1/2).
prim(my_tail2/2).
prim(my_last3/2).
prim(my_len4/2).
prim(my_reverse5/2).
prim(my_tail6/2).
prim(my_pred7/2).
prim(my_last8/2).
prim(my_last9/2).
prim(my_head10/2).
prim(my_tail11/2).
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
p([['k','u','k','e'],['u','x','q','d'],['c','j','e','n']],[['k','u','k'],['u','x','q'],['c','j','e']]).
p([['a','x','t'],['g','c','x'],['a','w','e','m']],[['a','x'],['g','c'],['a','w','e']]).
