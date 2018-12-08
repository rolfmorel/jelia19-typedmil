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
my_last1(A,B):-last(A,B).
my_last2(A,B):-last(A,B).
my_succ3(A,B):-succ(A,B).
my_head4([H|_],H).
my_pred5(A,B):-succ(B,A).
my_max_list6(A,B):-max_list(A,B).
my_min_list7(A,B):-min_list(A,B).
my_last8(A,B):-last(A,B).
my_reverse9(A,B):-reverse(A,B).
my_tail10([_|TL],TL).
my_sumlist11(A,B):-sumlist(A,B).
my_min_list12(A,B):-min_list(A,B).
my_succ13(A,B):-succ(A,B).
my_tail14([_|TL],TL).
my_reverse15(A,B):-reverse(A,B).
my_pred16(A,B):-succ(B,A).
my_last17(A,B):-last(A,B).
my_pred18(A,B):-succ(B,A).
my_pred19(A,B):-succ(B,A).
my_min_list20(A,B):-min_list(A,B).
my_pred21(A,B):-succ(B,A).
my_tail22([_|TL],TL).
my_succ23(A,B):-succ(A,B).
prim(my_reverse0/2).
prim(my_last1/2).
prim(my_last2/2).
prim(my_succ3/2).
prim(my_head4/2).
prim(my_pred5/2).
prim(my_max_list6/2).
prim(my_min_list7/2).
prim(my_last8/2).
prim(my_reverse9/2).
prim(my_tail10/2).
prim(my_sumlist11/2).
prim(my_min_list12/2).
prim(my_succ13/2).
prim(my_tail14/2).
prim(my_reverse15/2).
prim(my_pred16/2).
prim(my_last17/2).
prim(my_pred18/2).
prim(my_pred19/2).
prim(my_min_list20/2).
prim(my_pred21/2).
prim(my_tail22/2).
prim(my_succ23/2).
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
p([['a','y','c','o'],['a','l','q'],['i','t','c','f'],['n','r','t']],[['a','y','c'],['a','l'],['i','t','c'],['n','r']]).
p([['e','o','w'],['s','q','n']],[['e','o'],['s','q']]).
