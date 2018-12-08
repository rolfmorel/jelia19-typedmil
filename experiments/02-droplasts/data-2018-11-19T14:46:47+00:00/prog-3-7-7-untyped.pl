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
my_head0([H|_],H).
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_min_list3(A,B):-min_list(A,B).
my_succ4(A,B):-succ(A,B).
my_reverse5(A,B):-reverse(A,B).
my_reverse6(A,B):-reverse(A,B).
prim(my_head0/2).
prim(my_min_list1/2).
prim(my_succ2/2).
prim(my_min_list3/2).
prim(my_succ4/2).
prim(my_reverse5/2).
prim(my_reverse6/2).
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
p([['l','q','l','p'],['o','x','v','w'],['m','c','o'],['n','y','f','v']],[['l','q','l'],['o','x','v'],['m','c'],['n','y','f']]).
p([['m','o','d','c'],['t','f','p'],['i','g','i','f'],['x','i','t','v']],[['m','o','d'],['t','f'],['i','g','i'],['x','i','t']]).
