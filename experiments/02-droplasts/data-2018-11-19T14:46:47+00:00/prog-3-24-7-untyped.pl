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
my_tail0([_|TL],TL).
my_min_list1(A,B):-min_list(A,B).
my_min_list2(A,B):-min_list(A,B).
my_succ3(A,B):-succ(A,B).
my_max_list4(A,B):-max_list(A,B).
my_tail5([_|TL],TL).
my_len6(A,B):-length(A,B).
my_reverse7(A,B):-reverse(A,B).
my_min_list8(A,B):-min_list(A,B).
my_min_list9(A,B):-min_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_succ11(A,B):-succ(A,B).
my_reverse12(A,B):-reverse(A,B).
my_min_list13(A,B):-min_list(A,B).
my_last14(A,B):-last(A,B).
my_last15(A,B):-last(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_last17(A,B):-last(A,B).
my_succ18(A,B):-succ(A,B).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
my_last21(A,B):-last(A,B).
my_succ22(A,B):-succ(A,B).
my_max_list23(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_min_list1/2).
prim(my_min_list2/2).
prim(my_succ3/2).
prim(my_max_list4/2).
prim(my_tail5/2).
prim(my_len6/2).
prim(my_reverse7/2).
prim(my_min_list8/2).
prim(my_min_list9/2).
prim(my_max_list10/2).
prim(my_succ11/2).
prim(my_reverse12/2).
prim(my_min_list13/2).
prim(my_last14/2).
prim(my_last15/2).
prim(my_sumlist16/2).
prim(my_last17/2).
prim(my_succ18/2).
prim(my_pred19/2).
prim(my_pred20/2).
prim(my_last21/2).
prim(my_succ22/2).
prim(my_max_list23/2).
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
p([['s','h','a'],['g','s','k'],['w','l','e'],['x','k','f','a']],[['s','h'],['g','s'],['w','l'],['x','k','f']]).
p([['n','s','p','q'],['u','y','w']],[['n','s','p'],['u','y']]).
