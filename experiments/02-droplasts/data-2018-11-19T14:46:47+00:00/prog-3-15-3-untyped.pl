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
my_sumlist1(A,B):-sumlist(A,B).
my_reverse2(A,B):-reverse(A,B).
my_min_list3(A,B):-min_list(A,B).
my_succ4(A,B):-succ(A,B).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_reverse7(A,B):-reverse(A,B).
my_pred8(A,B):-succ(B,A).
my_sumlist9(A,B):-sumlist(A,B).
my_pred10(A,B):-succ(B,A).
my_min_list11(A,B):-min_list(A,B).
my_head12([H|_],H).
my_reverse13(A,B):-reverse(A,B).
my_tail14([_|TL],TL).
prim(my_max_list0/2).
prim(my_sumlist1/2).
prim(my_reverse2/2).
prim(my_min_list3/2).
prim(my_succ4/2).
prim(my_succ5/2).
prim(my_head6/2).
prim(my_reverse7/2).
prim(my_pred8/2).
prim(my_sumlist9/2).
prim(my_pred10/2).
prim(my_min_list11/2).
prim(my_head12/2).
prim(my_reverse13/2).
prim(my_tail14/2).
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
p([['d','k','s'],['i','r','v'],['n','i','a'],['v','h','w']],[['d','k'],['i','r'],['n','i'],['v','h']]).
p([['r','n','c'],['v','n','e','m']],[['r','n'],['v','n','e']]).
