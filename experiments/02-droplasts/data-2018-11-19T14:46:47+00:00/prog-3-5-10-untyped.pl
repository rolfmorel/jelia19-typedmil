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
my_len1(A,B):-length(A,B).
my_reverse2(A,B):-reverse(A,B).
my_tail3([_|TL],TL).
my_len4(A,B):-length(A,B).
prim(my_min_list0/2).
prim(my_len1/2).
prim(my_reverse2/2).
prim(my_tail3/2).
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
p([['e','r','i'],['s','y','f']],[['e','r'],['s','y']]).
p([['t','n','y','r'],['r','h','e','o'],['m','y','o'],['e','n','t']],[['t','n','y'],['r','h','e'],['m','y'],['e','n']]).
