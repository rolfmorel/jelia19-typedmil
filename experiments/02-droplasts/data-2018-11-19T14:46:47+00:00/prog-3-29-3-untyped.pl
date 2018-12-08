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
my_len1(A,B):-length(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_head4([H|_],H).
my_tail5([_|TL],TL).
my_sumlist6(A,B):-sumlist(A,B).
my_max_list7(A,B):-max_list(A,B).
my_pred8(A,B):-succ(B,A).
my_len9(A,B):-length(A,B).
my_min_list10(A,B):-min_list(A,B).
my_pred11(A,B):-succ(B,A).
my_tail12([_|TL],TL).
my_sumlist13(A,B):-sumlist(A,B).
my_tail14([_|TL],TL).
my_reverse15(A,B):-reverse(A,B).
my_min_list16(A,B):-min_list(A,B).
my_tail17([_|TL],TL).
my_pred18(A,B):-succ(B,A).
my_len19(A,B):-length(A,B).
my_pred20(A,B):-succ(B,A).
my_succ21(A,B):-succ(A,B).
my_min_list22(A,B):-min_list(A,B).
my_succ23(A,B):-succ(A,B).
my_pred24(A,B):-succ(B,A).
my_len25(A,B):-length(A,B).
my_succ26(A,B):-succ(A,B).
my_tail27([_|TL],TL).
my_pred28(A,B):-succ(B,A).
prim(my_pred0/2).
prim(my_len1/2).
prim(my_sumlist2/2).
prim(my_sumlist3/2).
prim(my_head4/2).
prim(my_tail5/2).
prim(my_sumlist6/2).
prim(my_max_list7/2).
prim(my_pred8/2).
prim(my_len9/2).
prim(my_min_list10/2).
prim(my_pred11/2).
prim(my_tail12/2).
prim(my_sumlist13/2).
prim(my_tail14/2).
prim(my_reverse15/2).
prim(my_min_list16/2).
prim(my_tail17/2).
prim(my_pred18/2).
prim(my_len19/2).
prim(my_pred20/2).
prim(my_succ21/2).
prim(my_min_list22/2).
prim(my_succ23/2).
prim(my_pred24/2).
prim(my_len25/2).
prim(my_succ26/2).
prim(my_tail27/2).
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
p([['i','b','r','c'],['t','h','s','j']],[['i','b','r'],['t','h','s']]).
p([['f','m','g'],['r','x','m','h'],['a','e','o','s'],['g','n','s','e']],[['f','m'],['r','x','m'],['a','e','o'],['g','n','s']]).
