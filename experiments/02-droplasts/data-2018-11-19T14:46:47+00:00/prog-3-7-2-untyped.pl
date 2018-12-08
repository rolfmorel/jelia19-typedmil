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
my_sumlist0(A,B):-sumlist(A,B).
my_tail1([_|TL],TL).
my_reverse2(A,B):-reverse(A,B).
my_succ3(A,B):-succ(A,B).
my_succ4(A,B):-succ(A,B).
my_last5(A,B):-last(A,B).
my_sumlist6(A,B):-sumlist(A,B).
prim(my_sumlist0/2).
prim(my_tail1/2).
prim(my_reverse2/2).
prim(my_succ3/2).
prim(my_succ4/2).
prim(my_last5/2).
prim(my_sumlist6/2).
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
p([['o','k','q','j'],['v','b','l','c'],['j','r','a','d']],[['o','k','q'],['v','b','l'],['j','r','a']]).
p([['d','w','w'],['f','l','j','b'],['q','q','u','p'],['a','g','b','o']],[['d','w'],['f','l','j'],['q','q','u'],['a','g','b']]).
