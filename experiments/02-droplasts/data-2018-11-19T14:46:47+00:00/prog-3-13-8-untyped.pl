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
my_tail2([_|TL],TL).
my_tail3([_|TL],TL).
my_last4(A,B):-last(A,B).
my_tail5([_|TL],TL).
my_min_list6(A,B):-min_list(A,B).
my_min_list7(A,B):-min_list(A,B).
my_max_list8(A,B):-max_list(A,B).
my_len9(A,B):-length(A,B).
my_head10([H|_],H).
my_max_list11(A,B):-max_list(A,B).
my_head12([H|_],H).
prim(my_head0/2).
prim(my_min_list1/2).
prim(my_tail2/2).
prim(my_tail3/2).
prim(my_last4/2).
prim(my_tail5/2).
prim(my_min_list6/2).
prim(my_min_list7/2).
prim(my_max_list8/2).
prim(my_len9/2).
prim(my_head10/2).
prim(my_max_list11/2).
prim(my_head12/2).
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
p([['n','g','y'],['n','o','n','y']],[['n','g'],['n','o','n']]).
p([['c','t','r'],['s','y','a','o'],['n','d','n']],[['c','t'],['s','y','a'],['n','d']]).
