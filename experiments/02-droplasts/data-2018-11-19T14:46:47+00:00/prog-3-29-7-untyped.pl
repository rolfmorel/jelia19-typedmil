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
my_tail2([_|TL],TL).
my_sumlist3(A,B):-sumlist(A,B).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_pred6(A,B):-succ(B,A).
my_succ7(A,B):-succ(A,B).
my_pred8(A,B):-succ(B,A).
my_sumlist9(A,B):-sumlist(A,B).
my_max_list10(A,B):-max_list(A,B).
my_head11([H|_],H).
my_head12([H|_],H).
my_pred13(A,B):-succ(B,A).
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_succ16(A,B):-succ(A,B).
my_min_list17(A,B):-min_list(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_head19([H|_],H).
my_last20(A,B):-last(A,B).
my_reverse21(A,B):-reverse(A,B).
my_len22(A,B):-length(A,B).
my_min_list23(A,B):-min_list(A,B).
my_head24([H|_],H).
my_succ25(A,B):-succ(A,B).
my_succ26(A,B):-succ(A,B).
my_tail27([_|TL],TL).
my_reverse28(A,B):-reverse(A,B).
prim(my_succ0/2).
prim(my_tail1/2).
prim(my_tail2/2).
prim(my_sumlist3/2).
prim(my_max_list4/2).
prim(my_head5/2).
prim(my_pred6/2).
prim(my_succ7/2).
prim(my_pred8/2).
prim(my_sumlist9/2).
prim(my_max_list10/2).
prim(my_head11/2).
prim(my_head12/2).
prim(my_pred13/2).
prim(my_max_list14/2).
prim(my_sumlist15/2).
prim(my_succ16/2).
prim(my_min_list17/2).
prim(my_sumlist18/2).
prim(my_head19/2).
prim(my_last20/2).
prim(my_reverse21/2).
prim(my_len22/2).
prim(my_min_list23/2).
prim(my_head24/2).
prim(my_succ25/2).
prim(my_succ26/2).
prim(my_tail27/2).
prim(my_reverse28/2).
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
p([['k','x','p'],['n','l','b']],[['k','x'],['n','l']]).
p([['c','c','m'],['q','c','j'],['y','n','v']],[['c','c'],['q','c'],['y','n']]).
