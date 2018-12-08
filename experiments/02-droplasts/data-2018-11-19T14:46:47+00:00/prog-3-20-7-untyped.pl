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
my_reverse1(A,B):-reverse(A,B).
my_max_list2(A,B):-max_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_tail4([_|TL],TL).
my_reverse5(A,B):-reverse(A,B).
my_min_list6(A,B):-min_list(A,B).
my_head7([H|_],H).
my_max_list8(A,B):-max_list(A,B).
my_head9([H|_],H).
my_head10([H|_],H).
my_last11(A,B):-last(A,B).
my_pred12(A,B):-succ(B,A).
my_tail13([_|TL],TL).
my_len14(A,B):-length(A,B).
my_max_list15(A,B):-max_list(A,B).
my_tail16([_|TL],TL).
my_len17(A,B):-length(A,B).
my_min_list18(A,B):-min_list(A,B).
my_succ19(A,B):-succ(A,B).
prim(my_min_list0/2).
prim(my_reverse1/2).
prim(my_max_list2/2).
prim(my_reverse3/2).
prim(my_tail4/2).
prim(my_reverse5/2).
prim(my_min_list6/2).
prim(my_head7/2).
prim(my_max_list8/2).
prim(my_head9/2).
prim(my_head10/2).
prim(my_last11/2).
prim(my_pred12/2).
prim(my_tail13/2).
prim(my_len14/2).
prim(my_max_list15/2).
prim(my_tail16/2).
prim(my_len17/2).
prim(my_min_list18/2).
prim(my_succ19/2).
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
p([['w','j','d'],['h','q','p'],['f','i','u'],['b','u','o']],[['w','j'],['h','q'],['f','i'],['b','u']]).
p([['d','g','s','w'],['m','i','n']],[['d','g','s'],['m','i']]).
