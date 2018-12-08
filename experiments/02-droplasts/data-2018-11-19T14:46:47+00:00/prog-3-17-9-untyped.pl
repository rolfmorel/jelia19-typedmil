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
my_reverse1(A,B):-reverse(A,B).
my_max_list2(A,B):-max_list(A,B).
my_pred3(A,B):-succ(B,A).
my_last4(A,B):-last(A,B).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_pred7(A,B):-succ(B,A).
my_reverse8(A,B):-reverse(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_reverse10(A,B):-reverse(A,B).
my_min_list11(A,B):-min_list(A,B).
my_head12([H|_],H).
my_succ13(A,B):-succ(A,B).
my_min_list14(A,B):-min_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_reverse16(A,B):-reverse(A,B).
prim(my_pred0/2).
prim(my_reverse1/2).
prim(my_max_list2/2).
prim(my_pred3/2).
prim(my_last4/2).
prim(my_succ5/2).
prim(my_head6/2).
prim(my_pred7/2).
prim(my_reverse8/2).
prim(my_sumlist9/2).
prim(my_reverse10/2).
prim(my_min_list11/2).
prim(my_head12/2).
prim(my_succ13/2).
prim(my_min_list14/2).
prim(my_min_list15/2).
prim(my_reverse16/2).
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
p([['m','u','r','q'],['o','h','k'],['c','b','p','n']],[['m','u','r'],['o','h'],['c','b','p']]).
p([['f','k','e'],['b','y','v']],[['f','k'],['b','y']]).
