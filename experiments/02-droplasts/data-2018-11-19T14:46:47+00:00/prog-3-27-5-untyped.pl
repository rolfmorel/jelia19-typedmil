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
my_head0([H|_],H).
my_min_list1(A,B):-min_list(A,B).
my_pred2(A,B):-succ(B,A).
my_succ3(A,B):-succ(A,B).
my_last4(A,B):-last(A,B).
my_min_list5(A,B):-min_list(A,B).
my_succ6(A,B):-succ(A,B).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B).
my_len9(A,B):-length(A,B).
my_succ10(A,B):-succ(A,B).
my_last11(A,B):-last(A,B).
my_reverse12(A,B):-reverse(A,B).
my_tail13([_|TL],TL).
my_tail14([_|TL],TL).
my_pred15(A,B):-succ(B,A).
my_tail16([_|TL],TL).
my_head17([H|_],H).
my_max_list18(A,B):-max_list(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_reverse21(A,B):-reverse(A,B).
my_head22([H|_],H).
my_max_list23(A,B):-max_list(A,B).
my_len24(A,B):-length(A,B).
my_last25(A,B):-last(A,B).
my_pred26(A,B):-succ(B,A).
prim(my_head0/2).
prim(my_min_list1/2).
prim(my_pred2/2).
prim(my_succ3/2).
prim(my_last4/2).
prim(my_min_list5/2).
prim(my_succ6/2).
prim(my_reverse7/2).
prim(my_succ8/2).
prim(my_len9/2).
prim(my_succ10/2).
prim(my_last11/2).
prim(my_reverse12/2).
prim(my_tail13/2).
prim(my_tail14/2).
prim(my_pred15/2).
prim(my_tail16/2).
prim(my_head17/2).
prim(my_max_list18/2).
prim(my_sumlist19/2).
prim(my_sumlist20/2).
prim(my_reverse21/2).
prim(my_head22/2).
prim(my_max_list23/2).
prim(my_len24/2).
prim(my_last25/2).
prim(my_pred26/2).
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
p([['a','q','a','i'],['t','c','f']],[['a','q','a'],['t','c']]).
p([['g','u','g','p'],['u','u','j','i'],['c','m','p','e'],['v','l','d']],[['g','u','g'],['u','u','j'],['c','m','p'],['v','l']]).
