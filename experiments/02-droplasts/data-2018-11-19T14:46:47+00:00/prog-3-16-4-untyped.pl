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
my_sumlist1(A,B):-sumlist(A,B).
my_succ2(A,B):-succ(A,B).
my_min_list3(A,B):-min_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_last5(A,B):-last(A,B).
my_last6(A,B):-last(A,B).
my_succ7(A,B):-succ(A,B).
my_max_list8(A,B):-max_list(A,B).
my_tail9([_|TL],TL).
my_succ10(A,B):-succ(A,B).
my_succ11(A,B):-succ(A,B).
my_reverse12(A,B):-reverse(A,B).
my_head13([H|_],H).
my_last14(A,B):-last(A,B).
my_succ15(A,B):-succ(A,B).
prim(my_succ0/2).
prim(my_sumlist1/2).
prim(my_succ2/2).
prim(my_min_list3/2).
prim(my_max_list4/2).
prim(my_last5/2).
prim(my_last6/2).
prim(my_succ7/2).
prim(my_max_list8/2).
prim(my_tail9/2).
prim(my_succ10/2).
prim(my_succ11/2).
prim(my_reverse12/2).
prim(my_head13/2).
prim(my_last14/2).
prim(my_succ15/2).
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
p([['e','j','r'],['x','i','u'],['d','k','f','p'],['f','e','d']],[['e','j'],['x','i'],['d','k','f'],['f','e']]).
p([['w','e','o','a'],['h','b','r'],['x','n','i','r'],['y','e','c','j']],[['w','e','o'],['h','b'],['x','n','i'],['y','e','c']]).
