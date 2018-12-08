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
my_succ0(A,B):-succ(A,B).
my_max_list1(A,B):-max_list(A,B).
my_max_list2(A,B):-max_list(A,B).
my_min_list3(A,B):-min_list(A,B).
my_head4([H|_],H).
my_tail5([_|TL],TL).
my_last6(A,B):-last(A,B).
my_head7([H|_],H).
my_tail8([_|TL],TL).
my_head9([H|_],H).
my_max_list10(A,B):-max_list(A,B).
my_max_list11(A,B):-max_list(A,B).
my_min_list12(A,B):-min_list(A,B).
my_succ13(A,B):-succ(A,B).
my_last14(A,B):-last(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_tail16([_|TL],TL).
my_max_list17(A,B):-max_list(A,B).
prim(my_succ0/2).
prim(my_max_list1/2).
prim(my_max_list2/2).
prim(my_min_list3/2).
prim(my_head4/2).
prim(my_tail5/2).
prim(my_last6/2).
prim(my_head7/2).
prim(my_tail8/2).
prim(my_head9/2).
prim(my_max_list10/2).
prim(my_max_list11/2).
prim(my_min_list12/2).
prim(my_succ13/2).
prim(my_last14/2).
prim(my_sumlist15/2).
prim(my_tail16/2).
prim(my_max_list17/2).
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
p([['k','l','q'],['u','v','t'],['s','j','r','n']],[['k','l'],['u','v'],['s','j','r']]).
p([['e','j','e'],['s','m','m'],['m','n','p','c']],[['e','j'],['s','m'],['m','n','p']]).
