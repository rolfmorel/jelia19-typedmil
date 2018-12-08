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
my_last1(A,B):-last(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_head3([H|_],H).
my_reverse4(A,B):-reverse(A,B).
prim(my_reverse0/2).
prim(my_last1/2).
prim(my_sumlist2/2).
prim(my_head3/2).
prim(my_reverse4/2).
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
p([['n','j','t','t'],['m','e','g'],['v','h','r','g']],[['n','j','t'],['m','e'],['v','h','r']]).
p([['b','g','b','v'],['g','k','f'],['f','i','v','b']],[['b','g','b'],['g','k'],['f','i','v']]).
