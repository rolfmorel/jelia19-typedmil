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
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_succ4(A,B):-succ(A,B).
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_min_list7(A,B):-min_list(A,B).
my_succ8(A,B):-succ(A,B).
my_reverse9(A,B):-reverse(A,B).
my_tail10([_|TL],TL).
my_max_list11(A,B):-max_list(A,B).
my_pred12(A,B):-succ(B,A).
my_min_list13(A,B):-min_list(A,B).
my_len14(A,B):-length(A,B).
my_min_list15(A,B):-min_list(A,B).
my_pred16(A,B):-succ(B,A).
my_last17(A,B):-last(A,B).
my_head18([H|_],H).
my_reverse19(A,B):-reverse(A,B).
my_last20(A,B):-last(A,B).
prim(my_pred0/2).
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_last3/2).
prim(my_succ4/2).
prim(my_last5/2).
prim(my_min_list6/2).
prim(my_min_list7/2).
prim(my_succ8/2).
prim(my_reverse9/2).
prim(my_tail10/2).
prim(my_max_list11/2).
prim(my_pred12/2).
prim(my_min_list13/2).
prim(my_len14/2).
prim(my_min_list15/2).
prim(my_pred16/2).
prim(my_last17/2).
prim(my_head18/2).
prim(my_reverse19/2).
prim(my_last20/2).
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
p([['l','a','l','l'],['w','v','p','g'],['j','b','f','e'],['u','o','p']],[['l','a','l'],['w','v','p'],['j','b','f'],['u','o']]).
p([['h','r','b'],['t','e','g']],[['h','r'],['t','e']]).
