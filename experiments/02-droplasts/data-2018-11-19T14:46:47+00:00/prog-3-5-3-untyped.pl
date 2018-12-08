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
my_min_list0(A,B):-min_list(A,B).
my_tail1([_|TL],TL).
my_len2(A,B):-length(A,B).
my_head3([H|_],H).
my_len4(A,B):-length(A,B).
prim(my_min_list0/2).
prim(my_tail1/2).
prim(my_len2/2).
prim(my_head3/2).
prim(my_len4/2).
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
p([['m','l','s'],['t','l','t','t'],['p','p','u','q']],[['m','l'],['t','l','t'],['p','p','u']]).
p([['i','n','k','r'],['k','w','q'],['s','c','m','w']],[['i','n','k'],['k','w'],['s','c','m']]).
