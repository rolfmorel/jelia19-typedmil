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
my_max_list0(A,B):-max_list(A,B).
my_last1(A,B):-last(A,B).
my_min_list2(A,B):-min_list(A,B).
my_head3([H|_],H).
my_pred4(A,B):-succ(B,A).
my_tail5([_|TL],TL).
my_last6(A,B):-last(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_max_list8(A,B):-max_list(A,B).
my_pred9(A,B):-succ(B,A).
my_succ10(A,B):-succ(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_succ12(A,B):-succ(A,B).
my_sumlist13(A,B):-sumlist(A,B).
prim(my_max_list0/2).
prim(my_last1/2).
prim(my_min_list2/2).
prim(my_head3/2).
prim(my_pred4/2).
prim(my_tail5/2).
prim(my_last6/2).
prim(my_sumlist7/2).
prim(my_max_list8/2).
prim(my_pred9/2).
prim(my_succ10/2).
prim(my_sumlist11/2).
prim(my_succ12/2).
prim(my_sumlist13/2).
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
p([['g','h','t'],['q','k','j']],[['g','h'],['q','k']]).
p([['x','t','u','c'],['m','j','f'],['d','k','s']],[['x','t','u'],['m','j'],['d','k']]).
