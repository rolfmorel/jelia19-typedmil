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
my_len0(A,B):-length(A,B).
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_succ3(A,B):-succ(A,B).
my_last4(A,B):-last(A,B).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_tail7([_|TL],TL).
my_min_list8(A,B):-min_list(A,B).
my_tail9([_|TL],TL).
my_tail10([_|TL],TL).
my_reverse11(A,B):-reverse(A,B).
my_tail12([_|TL],TL).
my_pred13(A,B):-succ(B,A).
my_max_list14(A,B):-max_list(A,B).
my_reverse15(A,B):-reverse(A,B).
my_len16(A,B):-length(A,B).
my_min_list17(A,B):-min_list(A,B).
my_tail18([_|TL],TL).
my_last19(A,B):-last(A,B).
my_tail20([_|TL],TL).
my_len21(A,B):-length(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_pred23(A,B):-succ(B,A).
my_pred24(A,B):-succ(B,A).
my_tail25([_|TL],TL).
my_sumlist26(A,B):-sumlist(A,B).
my_max_list27(A,B):-max_list(A,B).
my_pred28(A,B):-succ(B,A).
prim(my_len0/2).
prim(my_min_list1/2).
prim(my_succ2/2).
prim(my_succ3/2).
prim(my_last4/2).
prim(my_len5/2).
prim(my_min_list6/2).
prim(my_tail7/2).
prim(my_min_list8/2).
prim(my_tail9/2).
prim(my_tail10/2).
prim(my_reverse11/2).
prim(my_tail12/2).
prim(my_pred13/2).
prim(my_max_list14/2).
prim(my_reverse15/2).
prim(my_len16/2).
prim(my_min_list17/2).
prim(my_tail18/2).
prim(my_last19/2).
prim(my_tail20/2).
prim(my_len21/2).
prim(my_sumlist22/2).
prim(my_pred23/2).
prim(my_pred24/2).
prim(my_tail25/2).
prim(my_sumlist26/2).
prim(my_max_list27/2).
prim(my_pred28/2).
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
p([['p','e','q'],['l','l','s']],[['p','e'],['l','l']]).
p([['b','g','x'],['i','r','b']],[['b','g'],['i','r']]).
