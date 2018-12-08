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
my_last1(A,B):-last(A,B).
my_tail2([_|TL],TL).
my_len3(A,B):-length(A,B).
my_tail4([_|TL],TL).
my_max_list5(A,B):-max_list(A,B).
my_max_list6(A,B):-max_list(A,B).
my_max_list7(A,B):-max_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_min_list9(A,B):-min_list(A,B).
my_last10(A,B):-last(A,B).
prim(my_tail0/2).
prim(my_last1/2).
prim(my_tail2/2).
prim(my_len3/2).
prim(my_tail4/2).
prim(my_max_list5/2).
prim(my_max_list6/2).
prim(my_max_list7/2).
prim(my_reverse8/2).
prim(my_min_list9/2).
prim(my_last10/2).
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
p([['v','a','g'],['a','y','d']],[['v','a'],['a','y']]).
p([['h','p','n','p'],['p','n','x'],['v','p','p']],[['h','p','n'],['p','n'],['v','p']]).
