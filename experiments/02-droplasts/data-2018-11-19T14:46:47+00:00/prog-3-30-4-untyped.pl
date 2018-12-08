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
my_succ2(A,B):-succ(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_succ5(A,B):-succ(A,B).
my_min_list6(A,B):-min_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B).
my_pred9(A,B):-succ(B,A).
my_head10([H|_],H).
my_min_list11(A,B):-min_list(A,B).
my_head12([H|_],H).
my_head13([H|_],H).
my_len14(A,B):-length(A,B).
my_head15([H|_],H).
my_max_list16(A,B):-max_list(A,B).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_succ19(A,B):-succ(A,B).
my_reverse20(A,B):-reverse(A,B).
my_min_list21(A,B):-min_list(A,B).
my_len22(A,B):-length(A,B).
my_min_list23(A,B):-min_list(A,B).
my_len24(A,B):-length(A,B).
my_head25([H|_],H).
my_tail26([_|TL],TL).
my_last27(A,B):-last(A,B).
my_min_list28(A,B):-min_list(A,B).
my_head29([H|_],H).
prim(my_max_list0/2).
prim(my_max_list1/2).
prim(my_succ2/2).
prim(my_sumlist3/2).
prim(my_sumlist4/2).
prim(my_succ5/2).
prim(my_min_list6/2).
prim(my_reverse7/2).
prim(my_succ8/2).
prim(my_pred9/2).
prim(my_head10/2).
prim(my_min_list11/2).
prim(my_head12/2).
prim(my_head13/2).
prim(my_len14/2).
prim(my_head15/2).
prim(my_max_list16/2).
prim(my_head17/2).
prim(my_sumlist18/2).
prim(my_succ19/2).
prim(my_reverse20/2).
prim(my_min_list21/2).
prim(my_len22/2).
prim(my_min_list23/2).
prim(my_len24/2).
prim(my_head25/2).
prim(my_tail26/2).
prim(my_last27/2).
prim(my_min_list28/2).
prim(my_head29/2).
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
p([['y','q','t','x'],['d','g','a'],['q','n','e','j'],['a','i','d']],[['y','q','t'],['d','g'],['q','n','e'],['a','i']]).
p([['j','c','q'],['h','b','v'],['j','l','j','x']],[['j','c'],['h','b'],['j','l','j']]).
