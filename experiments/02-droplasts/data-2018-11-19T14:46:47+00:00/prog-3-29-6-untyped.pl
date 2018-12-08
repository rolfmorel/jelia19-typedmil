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
my_head1([H|_],H).
my_max_list2(A,B):-max_list(A,B).
my_succ3(A,B):-succ(A,B).
my_head4([H|_],H).
my_min_list5(A,B):-min_list(A,B).
my_succ6(A,B):-succ(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_reverse8(A,B):-reverse(A,B).
my_min_list9(A,B):-min_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_succ11(A,B):-succ(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_reverse13(A,B):-reverse(A,B).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
my_len18(A,B):-length(A,B).
my_head19([H|_],H).
my_min_list20(A,B):-min_list(A,B).
my_head21([H|_],H).
my_min_list22(A,B):-min_list(A,B).
my_len23(A,B):-length(A,B).
my_len24(A,B):-length(A,B).
my_tail25([_|TL],TL).
my_max_list26(A,B):-max_list(A,B).
my_pred27(A,B):-succ(B,A).
my_head28([H|_],H).
prim(my_pred0/2).
prim(my_head1/2).
prim(my_max_list2/2).
prim(my_succ3/2).
prim(my_head4/2).
prim(my_min_list5/2).
prim(my_succ6/2).
prim(my_sumlist7/2).
prim(my_reverse8/2).
prim(my_min_list9/2).
prim(my_max_list10/2).
prim(my_succ11/2).
prim(my_sumlist12/2).
prim(my_reverse13/2).
prim(my_succ14/2).
prim(my_last15/2).
prim(my_reverse16/2).
prim(my_max_list17/2).
prim(my_len18/2).
prim(my_head19/2).
prim(my_min_list20/2).
prim(my_head21/2).
prim(my_min_list22/2).
prim(my_len23/2).
prim(my_len24/2).
prim(my_tail25/2).
prim(my_max_list26/2).
prim(my_pred27/2).
prim(my_head28/2).
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
p([['n','c','r','f'],['r','k','l']],[['n','c','r'],['r','k']]).
p([['v','d','i','l'],['c','m','s'],['i','q','g']],[['v','d','i'],['c','m'],['i','q']]).
