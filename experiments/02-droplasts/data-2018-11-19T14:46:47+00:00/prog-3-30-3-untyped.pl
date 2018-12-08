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
my_reverse0(A,B):-reverse(A,B).
my_head1([H|_],H).
my_reverse2(A,B):-reverse(A,B).
my_reverse3(A,B):-reverse(A,B).
my_pred4(A,B):-succ(B,A).
my_reverse5(A,B):-reverse(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_last7(A,B):-last(A,B).
my_head8([H|_],H).
my_head9([H|_],H).
my_tail10([_|TL],TL).
my_reverse11(A,B):-reverse(A,B).
my_max_list12(A,B):-max_list(A,B).
my_reverse13(A,B):-reverse(A,B).
my_max_list14(A,B):-max_list(A,B).
my_last15(A,B):-last(A,B).
my_max_list16(A,B):-max_list(A,B).
my_succ17(A,B):-succ(A,B).
my_tail18([_|TL],TL).
my_head19([H|_],H).
my_tail20([_|TL],TL).
my_sumlist21(A,B):-sumlist(A,B).
my_succ22(A,B):-succ(A,B).
my_succ23(A,B):-succ(A,B).
my_max_list24(A,B):-max_list(A,B).
my_head25([H|_],H).
my_min_list26(A,B):-min_list(A,B).
my_head27([H|_],H).
my_pred28(A,B):-succ(B,A).
my_head29([H|_],H).
prim(my_reverse0/2).
prim(my_head1/2).
prim(my_reverse2/2).
prim(my_reverse3/2).
prim(my_pred4/2).
prim(my_reverse5/2).
prim(my_sumlist6/2).
prim(my_last7/2).
prim(my_head8/2).
prim(my_head9/2).
prim(my_tail10/2).
prim(my_reverse11/2).
prim(my_max_list12/2).
prim(my_reverse13/2).
prim(my_max_list14/2).
prim(my_last15/2).
prim(my_max_list16/2).
prim(my_succ17/2).
prim(my_tail18/2).
prim(my_head19/2).
prim(my_tail20/2).
prim(my_sumlist21/2).
prim(my_succ22/2).
prim(my_succ23/2).
prim(my_max_list24/2).
prim(my_head25/2).
prim(my_min_list26/2).
prim(my_head27/2).
prim(my_pred28/2).
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
p([['i','l','d'],['x','s','v','p'],['j','n','x','h'],['d','y','o','e']],[['i','l'],['x','s','v'],['j','n','x'],['d','y','o']]).
p([['d','j','c','a'],['f','e','w','u'],['c','q','q'],['b','c','x','q']],[['d','j','c'],['f','e','w'],['c','q'],['b','c','x']]).
