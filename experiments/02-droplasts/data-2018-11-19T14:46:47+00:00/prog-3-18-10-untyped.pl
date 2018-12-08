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
my_last0(A,B):-last(A,B).
my_last1(A,B):-last(A,B).
my_min_list2(A,B):-min_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_succ5(A,B):-succ(A,B).
my_reverse6(A,B):-reverse(A,B).
my_max_list7(A,B):-max_list(A,B).
my_max_list8(A,B):-max_list(A,B).
my_reverse9(A,B):-reverse(A,B).
my_len10(A,B):-length(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_succ12(A,B):-succ(A,B).
my_tail13([_|TL],TL).
my_tail14([_|TL],TL).
my_reverse15(A,B):-reverse(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_sumlist17(A,B):-sumlist(A,B).
prim(my_last0/2).
prim(my_last1/2).
prim(my_min_list2/2).
prim(my_max_list3/2).
prim(my_max_list4/2).
prim(my_succ5/2).
prim(my_reverse6/2).
prim(my_max_list7/2).
prim(my_max_list8/2).
prim(my_reverse9/2).
prim(my_len10/2).
prim(my_sumlist11/2).
prim(my_succ12/2).
prim(my_tail13/2).
prim(my_tail14/2).
prim(my_reverse15/2).
prim(my_sumlist16/2).
prim(my_sumlist17/2).
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
p([['w','f','c'],['d','q','n']],[['w','f'],['d','q']]).
p([['d','w','t','k'],['e','b','f','x'],['c','g','u','a']],[['d','w','t'],['e','b','f'],['c','g','u']]).
