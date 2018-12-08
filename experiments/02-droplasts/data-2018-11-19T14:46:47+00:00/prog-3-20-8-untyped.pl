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
my_max_list1(A,B):-max_list(A,B).
my_head2([H|_],H).
my_succ3(A,B):-succ(A,B).
my_succ4(A,B):-succ(A,B).
my_min_list5(A,B):-min_list(A,B).
my_len6(A,B):-length(A,B).
my_succ7(A,B):-succ(A,B).
my_tail8([_|TL],TL).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_tail11([_|TL],TL).
my_min_list12(A,B):-min_list(A,B).
my_last13(A,B):-last(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_min_list15(A,B):-min_list(A,B).
my_pred16(A,B):-succ(B,A).
my_len17(A,B):-length(A,B).
my_last18(A,B):-last(A,B).
my_len19(A,B):-length(A,B).
prim(my_max_list0/2).
prim(my_max_list1/2).
prim(my_head2/2).
prim(my_succ3/2).
prim(my_succ4/2).
prim(my_min_list5/2).
prim(my_len6/2).
prim(my_succ7/2).
prim(my_tail8/2).
prim(my_last9/2).
prim(my_head10/2).
prim(my_tail11/2).
prim(my_min_list12/2).
prim(my_last13/2).
prim(my_sumlist14/2).
prim(my_min_list15/2).
prim(my_pred16/2).
prim(my_len17/2).
prim(my_last18/2).
prim(my_len19/2).
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
p([['m','b','o','f'],['f','e','v','h']],[['m','b','o'],['f','e','v']]).
p([['i','c','j'],['a','k','j','k']],[['i','c'],['a','k','j']]).
