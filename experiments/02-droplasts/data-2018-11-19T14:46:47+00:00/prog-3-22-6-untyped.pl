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
my_reverse1(A,B):-reverse(A,B).
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_pred6(A,B):-succ(B,A).
my_succ7(A,B):-succ(A,B).
my_last8(A,B):-last(A,B).
my_max_list9(A,B):-max_list(A,B).
my_succ10(A,B):-succ(A,B).
my_pred11(A,B):-succ(B,A).
my_reverse12(A,B):-reverse(A,B).
my_pred13(A,B):-succ(B,A).
my_len14(A,B):-length(A,B).
my_head15([H|_],H).
my_len16(A,B):-length(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_tail18([_|TL],TL).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
my_len21(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_min_list2/2).
prim(my_last3/2).
prim(my_max_list4/2).
prim(my_head5/2).
prim(my_pred6/2).
prim(my_succ7/2).
prim(my_last8/2).
prim(my_max_list9/2).
prim(my_succ10/2).
prim(my_pred11/2).
prim(my_reverse12/2).
prim(my_pred13/2).
prim(my_len14/2).
prim(my_head15/2).
prim(my_len16/2).
prim(my_sumlist17/2).
prim(my_tail18/2).
prim(my_pred19/2).
prim(my_pred20/2).
prim(my_len21/2).
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
p([['x','j','r','j'],['n','q','n'],['x','c','b','i'],['v','y','u']],[['x','j','r'],['n','q'],['x','c','b'],['v','y']]).
p([['w','d','j','i'],['n','o','j'],['a','x','k','y']],[['w','d','j'],['n','o'],['a','x','k']]).
