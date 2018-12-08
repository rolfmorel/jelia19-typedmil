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
my_head2([H|_],H).
my_sumlist3(A,B):-sumlist(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_pred6(A,B):-succ(B,A).
my_pred7(A,B):-succ(B,A).
my_reverse8(A,B):-reverse(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_len10(A,B):-length(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_min_list14(A,B):-min_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_max_list16(A,B):-max_list(A,B).
my_min_list17(A,B):-min_list(A,B).
my_head18([H|_],H).
my_tail19([_|TL],TL).
my_pred20(A,B):-succ(B,A).
my_max_list21(A,B):-max_list(A,B).
my_len22(A,B):-length(A,B).
my_last23(A,B):-last(A,B).
my_len24(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_min_list1/2).
prim(my_head2/2).
prim(my_sumlist3/2).
prim(my_max_list4/2).
prim(my_len5/2).
prim(my_pred6/2).
prim(my_pred7/2).
prim(my_reverse8/2).
prim(my_sumlist9/2).
prim(my_len10/2).
prim(my_sumlist11/2).
prim(my_pred12/2).
prim(my_max_list13/2).
prim(my_min_list14/2).
prim(my_min_list15/2).
prim(my_max_list16/2).
prim(my_min_list17/2).
prim(my_head18/2).
prim(my_tail19/2).
prim(my_pred20/2).
prim(my_max_list21/2).
prim(my_len22/2).
prim(my_last23/2).
prim(my_len24/2).
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
p([['d','u','h'],['c','a','u','d']],[['d','u'],['c','a','u']]).
p([['p','a','l'],['g','f','q']],[['p','a'],['g','f']]).
