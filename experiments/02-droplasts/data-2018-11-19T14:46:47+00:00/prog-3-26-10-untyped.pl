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
my_sumlist1(A,B):-sumlist(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_min_list3(A,B):-min_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_tail5([_|TL],TL).
my_len6(A,B):-length(A,B).
my_min_list7(A,B):-min_list(A,B).
my_head8([H|_],H).
my_reverse9(A,B):-reverse(A,B).
my_succ10(A,B):-succ(A,B).
my_succ11(A,B):-succ(A,B).
my_len12(A,B):-length(A,B).
my_max_list13(A,B):-max_list(A,B).
my_max_list14(A,B):-max_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_min_list16(A,B):-min_list(A,B).
my_reverse17(A,B):-reverse(A,B).
my_succ18(A,B):-succ(A,B).
my_reverse19(A,B):-reverse(A,B).
my_len20(A,B):-length(A,B).
my_max_list21(A,B):-max_list(A,B).
my_len22(A,B):-length(A,B).
my_succ23(A,B):-succ(A,B).
my_max_list24(A,B):-max_list(A,B).
my_pred25(A,B):-succ(B,A).
prim(my_tail0/2).
prim(my_sumlist1/2).
prim(my_sumlist2/2).
prim(my_min_list3/2).
prim(my_min_list4/2).
prim(my_tail5/2).
prim(my_len6/2).
prim(my_min_list7/2).
prim(my_head8/2).
prim(my_reverse9/2).
prim(my_succ10/2).
prim(my_succ11/2).
prim(my_len12/2).
prim(my_max_list13/2).
prim(my_max_list14/2).
prim(my_min_list15/2).
prim(my_min_list16/2).
prim(my_reverse17/2).
prim(my_succ18/2).
prim(my_reverse19/2).
prim(my_len20/2).
prim(my_max_list21/2).
prim(my_len22/2).
prim(my_succ23/2).
prim(my_max_list24/2).
prim(my_pred25/2).
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
p([['e','f','l','y'],['i','t','v'],['h','f','w','x'],['l','k','g','e']],[['e','f','l'],['i','t'],['h','f','w'],['l','k','g']]).
p([['d','m','i'],['l','b','s'],['h','k','n','x'],['a','g','h']],[['d','m'],['l','b'],['h','k','n'],['a','g']]).
