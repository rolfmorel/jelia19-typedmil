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
my_tail1([_|TL],TL).
my_last2(A,B):-last(A,B).
my_len3(A,B):-length(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_pred6(A,B):-succ(B,A).
my_pred7(A,B):-succ(B,A).
my_last8(A,B):-last(A,B).
my_pred9(A,B):-succ(B,A).
my_sumlist10(A,B):-sumlist(A,B).
my_min_list11(A,B):-min_list(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_len13(A,B):-length(A,B).
my_pred14(A,B):-succ(B,A).
my_succ15(A,B):-succ(A,B).
my_min_list16(A,B):-min_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_tail18([_|TL],TL).
my_succ19(A,B):-succ(A,B).
my_last20(A,B):-last(A,B).
my_max_list21(A,B):-max_list(A,B).
my_max_list22(A,B):-max_list(A,B).
prim(my_succ0/2).
prim(my_tail1/2).
prim(my_last2/2).
prim(my_len3/2).
prim(my_max_list4/2).
prim(my_len5/2).
prim(my_pred6/2).
prim(my_pred7/2).
prim(my_last8/2).
prim(my_pred9/2).
prim(my_sumlist10/2).
prim(my_min_list11/2).
prim(my_sumlist12/2).
prim(my_len13/2).
prim(my_pred14/2).
prim(my_succ15/2).
prim(my_min_list16/2).
prim(my_sumlist17/2).
prim(my_tail18/2).
prim(my_succ19/2).
prim(my_last20/2).
prim(my_max_list21/2).
prim(my_max_list22/2).
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
p([['l','t','j','x'],['o','d','e'],['k','v','a','n']],[['l','t','j'],['o','d'],['k','v','a']]).
p([['u','s','v'],['w','l','m','w']],[['u','s'],['w','l','m']]).
