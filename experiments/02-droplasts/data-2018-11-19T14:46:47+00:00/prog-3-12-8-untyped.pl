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
my_pred1(A,B):-succ(B,A).
my_last2(A,B):-last(A,B).
my_tail3([_|TL],TL).
my_len4(A,B):-length(A,B).
my_min_list5(A,B):-min_list(A,B).
my_reverse6(A,B):-reverse(A,B).
my_len7(A,B):-length(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A).
my_head10([H|_],H).
my_head11([H|_],H).
prim(my_max_list0/2).
prim(my_pred1/2).
prim(my_last2/2).
prim(my_tail3/2).
prim(my_len4/2).
prim(my_min_list5/2).
prim(my_reverse6/2).
prim(my_len7/2).
prim(my_len8/2).
prim(my_pred9/2).
prim(my_head10/2).
prim(my_head11/2).
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
p([['i','d','g'],['r','h','y','f'],['o','r','k'],['b','i','p','r']],[['i','d'],['r','h','y'],['o','r'],['b','i','p']]).
p([['c','c','h','i'],['x','r','t']],[['c','c','h'],['x','r']]).
