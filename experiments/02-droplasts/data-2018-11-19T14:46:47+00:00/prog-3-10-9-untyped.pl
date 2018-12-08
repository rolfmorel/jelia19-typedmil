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
my_last1(A,B):-last(A,B).
my_last2(A,B):-last(A,B).
my_succ3(A,B):-succ(A,B).
my_pred4(A,B):-succ(B,A).
my_head5([H|_],H).
my_len6(A,B):-length(A,B).
my_head7([H|_],H).
my_min_list8(A,B):-min_list(A,B).
my_tail9([_|TL],TL).
prim(my_last0/2).
prim(my_last1/2).
prim(my_last2/2).
prim(my_succ3/2).
prim(my_pred4/2).
prim(my_head5/2).
prim(my_len6/2).
prim(my_head7/2).
prim(my_min_list8/2).
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
p([['y','h','n','l'],['g','o','p','y'],['b','h','c','n'],['p','m','o']],[['y','h','n'],['g','o','p'],['b','h','c'],['p','m']]).
p([['f','u','d','g'],['x','q','p','f']],[['f','u','d'],['x','q','p']]).
