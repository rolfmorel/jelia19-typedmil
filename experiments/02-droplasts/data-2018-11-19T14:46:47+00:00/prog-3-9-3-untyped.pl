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
my_tail0([_|TL],TL).
my_succ1(A,B):-succ(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_max_list3(A,B):-max_list(A,B).
my_len4(A,B):-length(A,B).
my_min_list5(A,B):-min_list(A,B).
my_min_list6(A,B):-min_list(A,B).
my_last7(A,B):-last(A,B).
my_head8([H|_],H).
prim(my_tail0/2).
prim(my_succ1/2).
prim(my_sumlist2/2).
prim(my_max_list3/2).
prim(my_len4/2).
prim(my_min_list5/2).
prim(my_min_list6/2).
prim(my_last7/2).
prim(my_head8/2).
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
p([['s','n','e'],['x','k','e'],['q','c','e','a']],[['s','n'],['x','k'],['q','c','e']]).
p([['l','n','h'],['r','d','y','s'],['l','u','y','m']],[['l','n'],['r','d','y'],['l','u','y']]).
