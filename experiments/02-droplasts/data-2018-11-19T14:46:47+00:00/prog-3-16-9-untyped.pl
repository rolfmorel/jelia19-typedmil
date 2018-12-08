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
my_len1(A,B):-length(A,B).
my_max_list2(A,B):-max_list(A,B).
my_min_list3(A,B):-min_list(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A).
my_pred10(A,B):-succ(B,A).
my_pred11(A,B):-succ(B,A).
my_len12(A,B):-length(A,B).
my_succ13(A,B):-succ(A,B).
my_min_list14(A,B):-min_list(A,B).
my_min_list15(A,B):-min_list(A,B).
prim(my_head0/2).
prim(my_len1/2).
prim(my_max_list2/2).
prim(my_min_list3/2).
prim(my_sumlist4/2).
prim(my_last5/2).
prim(my_min_list6/2).
prim(my_sumlist7/2).
prim(my_len8/2).
prim(my_pred9/2).
prim(my_pred10/2).
prim(my_pred11/2).
prim(my_len12/2).
prim(my_succ13/2).
prim(my_min_list14/2).
prim(my_min_list15/2).
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
p([['v','d','b'],['t','p','p'],['e','v','l'],['b','o','v']],[['v','d'],['t','p'],['e','v'],['b','o']]).
p([['s','o','n','y'],['d','m','o'],['e','t','q']],[['s','o','n'],['d','m'],['e','t']]).
