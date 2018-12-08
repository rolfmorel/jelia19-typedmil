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
my_reverse1(A,B):-reverse(A,B).
my_head2([H|_],H).
my_succ3(A,B):-succ(A,B).
prim(my_head0/2).
prim(my_reverse1/2).
prim(my_head2/2).
prim(my_succ3/2).
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
p([['g','c','s','a'],['j','n','x'],['a','i','n','f']],[['g','c','s'],['j','n'],['a','i','n']]).
p([['s','d','m'],['y','p','k'],['n','n','g','v'],['x','d','o','t']],[['s','d'],['y','p'],['n','n','g'],['x','d','o']]).
