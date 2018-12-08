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
my_sumlist1(A,B):-sumlist(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_max_list3(A,B):-max_list(A,B).
my_len4(A,B):-length(A,B).
my_max_list5(A,B):-max_list(A,B).
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_len8(A,B):-length(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_last11(A,B):-last(A,B).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_succ14(A,B):-succ(A,B).
my_head15([H|_],H).
my_succ16(A,B):-succ(A,B).
my_max_list17(A,B):-max_list(A,B).
my_max_list18(A,B):-max_list(A,B).
my_tail19([_|TL],TL).
my_reverse20(A,B):-reverse(A,B).
my_last21(A,B):-last(A,B).
prim(my_last0/2).
prim(my_sumlist1/2).
prim(my_sumlist2/2).
prim(my_max_list3/2).
prim(my_len4/2).
prim(my_max_list5/2).
prim(my_len6/2).
prim(my_max_list7/2).
prim(my_len8/2).
prim(my_last9/2).
prim(my_head10/2).
prim(my_last11/2).
prim(my_reverse12/2).
prim(my_len13/2).
prim(my_succ14/2).
prim(my_head15/2).
prim(my_succ16/2).
prim(my_max_list17/2).
prim(my_max_list18/2).
prim(my_tail19/2).
prim(my_reverse20/2).
prim(my_last21/2).
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
p([['b','v','n','f'],['p','i','g']],[['b','v','n'],['p','i']]).
p([['p','s','i','i'],['a','j','o'],['m','y','n','h']],[['p','s','i'],['a','j'],['m','y','n']]).
