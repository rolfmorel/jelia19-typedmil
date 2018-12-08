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
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_tail4([_|TL],TL).
my_max_list5(A,B):-max_list(A,B).
my_last6(A,B):-last(A,B).
my_head7([H|_],H).
my_min_list8(A,B):-min_list(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_head11([H|_],H).
my_reverse12(A,B):-reverse(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_max_list14(A,B):-max_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_pred16(A,B):-succ(B,A).
my_last17(A,B):-last(A,B).
my_succ18(A,B):-succ(A,B).
my_min_list19(A,B):-min_list(A,B).
my_max_list20(A,B):-max_list(A,B).
my_reverse21(A,B):-reverse(A,B).
my_succ22(A,B):-succ(A,B).
my_reverse23(A,B):-reverse(A,B).
my_pred24(A,B):-succ(B,A).
my_reverse25(A,B):-reverse(A,B).
my_succ26(A,B):-succ(A,B).
my_succ27(A,B):-succ(A,B).
prim(my_pred0/2).
prim(my_len1/2).
prim(my_min_list2/2).
prim(my_last3/2).
prim(my_tail4/2).
prim(my_max_list5/2).
prim(my_last6/2).
prim(my_head7/2).
prim(my_min_list8/2).
prim(my_last9/2).
prim(my_head10/2).
prim(my_head11/2).
prim(my_reverse12/2).
prim(my_sumlist13/2).
prim(my_max_list14/2).
prim(my_min_list15/2).
prim(my_pred16/2).
prim(my_last17/2).
prim(my_succ18/2).
prim(my_min_list19/2).
prim(my_max_list20/2).
prim(my_reverse21/2).
prim(my_succ22/2).
prim(my_reverse23/2).
prim(my_pred24/2).
prim(my_reverse25/2).
prim(my_succ26/2).
prim(my_succ27/2).
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
p([['d','w','r'],['h','o','q']],[['d','w'],['h','o']]).
p([['o','q','l','e'],['v','r','t']],[['o','q','l'],['v','r']]).
