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
my_reverse2(A,B):-reverse(A,B).
my_head3([H|_],H).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_succ6(A,B):-succ(A,B).
my_head7([H|_],H).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B).
my_last10(A,B):-last(A,B).
my_len11(A,B):-length(A,B).
my_min_list12(A,B):-min_list(A,B).
my_head13([H|_],H).
my_sumlist14(A,B):-sumlist(A,B).
my_head15([H|_],H).
my_last16(A,B):-last(A,B).
my_max_list17(A,B):-max_list(A,B).
my_last18(A,B):-last(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_pred20(A,B):-succ(B,A).
my_min_list21(A,B):-min_list(A,B).
my_head22([H|_],H).
my_sumlist23(A,B):-sumlist(A,B).
my_pred24(A,B):-succ(B,A).
my_reverse25(A,B):-reverse(A,B).
my_head26([H|_],H).
prim(my_succ0/2).
prim(my_max_list1/2).
prim(my_reverse2/2).
prim(my_head3/2).
prim(my_max_list4/2).
prim(my_head5/2).
prim(my_succ6/2).
prim(my_head7/2).
prim(my_len8/2).
prim(my_succ9/2).
prim(my_last10/2).
prim(my_len11/2).
prim(my_min_list12/2).
prim(my_head13/2).
prim(my_sumlist14/2).
prim(my_head15/2).
prim(my_last16/2).
prim(my_max_list17/2).
prim(my_last18/2).
prim(my_sumlist19/2).
prim(my_pred20/2).
prim(my_min_list21/2).
prim(my_head22/2).
prim(my_sumlist23/2).
prim(my_pred24/2).
prim(my_reverse25/2).
prim(my_head26/2).
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
p([['q','e','u','s'],['c','f','p'],['v','s','t','j'],['r','v','w']],[['q','e','u'],['c','f'],['v','s','t'],['r','v']]).
p([['s','d','u'],['u','c','x','f'],['o','c','p','i'],['u','o','b']],[['s','d'],['u','c','x'],['o','c','p'],['u','o']]).
