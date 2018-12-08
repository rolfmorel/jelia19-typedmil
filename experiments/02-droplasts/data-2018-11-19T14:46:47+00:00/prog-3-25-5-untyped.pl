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
my_pred0(A,B):-succ(B,A).
my_succ1(A,B):-succ(A,B).
my_reverse2(A,B):-reverse(A,B).
my_head3([H|_],H).
my_succ4(A,B):-succ(A,B).
my_tail5([_|TL],TL).
my_tail6([_|TL],TL).
my_last7(A,B):-last(A,B).
my_last8(A,B):-last(A,B).
my_succ9(A,B):-succ(A,B).
my_pred10(A,B):-succ(B,A).
my_pred11(A,B):-succ(B,A).
my_tail12([_|TL],TL).
my_head13([H|_],H).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_len16(A,B):-length(A,B).
my_succ17(A,B):-succ(A,B).
my_max_list18(A,B):-max_list(A,B).
my_tail19([_|TL],TL).
my_reverse20(A,B):-reverse(A,B).
my_pred21(A,B):-succ(B,A).
my_max_list22(A,B):-max_list(A,B).
my_sumlist23(A,B):-sumlist(A,B).
my_sumlist24(A,B):-sumlist(A,B).
prim(my_pred0/2).
prim(my_succ1/2).
prim(my_reverse2/2).
prim(my_head3/2).
prim(my_succ4/2).
prim(my_tail5/2).
prim(my_tail6/2).
prim(my_last7/2).
prim(my_last8/2).
prim(my_succ9/2).
prim(my_pred10/2).
prim(my_pred11/2).
prim(my_tail12/2).
prim(my_head13/2).
prim(my_succ14/2).
prim(my_last15/2).
prim(my_len16/2).
prim(my_succ17/2).
prim(my_max_list18/2).
prim(my_tail19/2).
prim(my_reverse20/2).
prim(my_pred21/2).
prim(my_max_list22/2).
prim(my_sumlist23/2).
prim(my_sumlist24/2).
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
p([['m','c','f','x'],['y','b','a'],['b','j','s'],['o','r','c']],[['m','c','f'],['y','b'],['b','j'],['o','r']]).
p([['p','t','k','j'],['h','g','f']],[['p','t','k'],['h','g']]).
