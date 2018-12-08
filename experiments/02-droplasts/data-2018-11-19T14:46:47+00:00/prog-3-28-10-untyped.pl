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
my_reverse1(A,B):-reverse(A,B).
my_reverse2(A,B):-reverse(A,B).
my_len3(A,B):-length(A,B).
my_len4(A,B):-length(A,B).
my_max_list5(A,B):-max_list(A,B).
my_len6(A,B):-length(A,B).
my_min_list7(A,B):-min_list(A,B).
my_tail8([_|TL],TL).
my_succ9(A,B):-succ(A,B).
my_head10([H|_],H).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_pred13(A,B):-succ(B,A).
my_succ14(A,B):-succ(A,B).
my_succ15(A,B):-succ(A,B).
my_reverse16(A,B):-reverse(A,B).
my_len17(A,B):-length(A,B).
my_last18(A,B):-last(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_succ20(A,B):-succ(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_head22([H|_],H).
my_min_list23(A,B):-min_list(A,B).
my_succ24(A,B):-succ(A,B).
my_min_list25(A,B):-min_list(A,B).
my_reverse26(A,B):-reverse(A,B).
my_head27([H|_],H).
prim(my_max_list0/2).
prim(my_reverse1/2).
prim(my_reverse2/2).
prim(my_len3/2).
prim(my_len4/2).
prim(my_max_list5/2).
prim(my_len6/2).
prim(my_min_list7/2).
prim(my_tail8/2).
prim(my_succ9/2).
prim(my_head10/2).
prim(my_sumlist11/2).
prim(my_last12/2).
prim(my_pred13/2).
prim(my_succ14/2).
prim(my_succ15/2).
prim(my_reverse16/2).
prim(my_len17/2).
prim(my_last18/2).
prim(my_sumlist19/2).
prim(my_succ20/2).
prim(my_sumlist21/2).
prim(my_head22/2).
prim(my_min_list23/2).
prim(my_succ24/2).
prim(my_min_list25/2).
prim(my_reverse26/2).
prim(my_head27/2).
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
p([['k','d','u'],['g','g','s','a']],[['k','d'],['g','g','s']]).
p([['x','l','d','j'],['f','i','w','f'],['n','q','c'],['b','k','g','q']],[['x','l','d'],['f','i','w'],['n','q'],['b','k','g']]).
