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
my_min_list0(A,B):-min_list(A,B).
my_head1([H|_],H).
my_min_list2(A,B):-min_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_pred4(A,B):-succ(B,A).
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
my_head7([H|_],H).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_len11(A,B):-length(A,B).
my_last12(A,B):-last(A,B).
my_min_list13(A,B):-min_list(A,B).
my_pred14(A,B):-succ(B,A).
my_max_list15(A,B):-max_list(A,B).
my_succ16(A,B):-succ(A,B).
my_last17(A,B):-last(A,B).
my_min_list18(A,B):-min_list(A,B).
my_tail19([_|TL],TL).
my_pred20(A,B):-succ(B,A).
my_len21(A,B):-length(A,B).
my_last22(A,B):-last(A,B).
my_len23(A,B):-length(A,B).
my_last24(A,B):-last(A,B).
my_len25(A,B):-length(A,B).
my_pred26(A,B):-succ(B,A).
my_len27(A,B):-length(A,B).
my_max_list28(A,B):-max_list(A,B).
prim(my_min_list0/2).
prim(my_head1/2).
prim(my_min_list2/2).
prim(my_reverse3/2).
prim(my_pred4/2).
prim(my_sumlist5/2).
prim(my_min_list6/2).
prim(my_head7/2).
prim(my_sumlist8/2).
prim(my_last9/2).
prim(my_head10/2).
prim(my_len11/2).
prim(my_last12/2).
prim(my_min_list13/2).
prim(my_pred14/2).
prim(my_max_list15/2).
prim(my_succ16/2).
prim(my_last17/2).
prim(my_min_list18/2).
prim(my_tail19/2).
prim(my_pred20/2).
prim(my_len21/2).
prim(my_last22/2).
prim(my_len23/2).
prim(my_last24/2).
prim(my_len25/2).
prim(my_pred26/2).
prim(my_len27/2).
prim(my_max_list28/2).
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
p([['j','m','a'],['w','e','c','x'],['c','l','y'],['u','f','v','x']],[['j','m'],['w','e','c'],['c','l'],['u','f','v']]).
p([['p','h','c'],['h','k','r'],['b','k','d'],['l','h','u','r']],[['p','h'],['h','k'],['b','k'],['l','h','u']]).
