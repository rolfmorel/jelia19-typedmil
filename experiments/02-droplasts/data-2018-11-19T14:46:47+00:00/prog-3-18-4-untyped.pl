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
my_succ1(A,B):-succ(A,B).
my_last2(A,B):-last(A,B).
my_succ3(A,B):-succ(A,B).
my_last4(A,B):-last(A,B).
my_last5(A,B):-last(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_max_list8(A,B):-max_list(A,B).
my_len9(A,B):-length(A,B).
my_succ10(A,B):-succ(A,B).
my_last11(A,B):-last(A,B).
my_len12(A,B):-length(A,B).
my_pred13(A,B):-succ(B,A).
my_reverse14(A,B):-reverse(A,B).
my_tail15([_|TL],TL).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
prim(my_min_list0/2).
prim(my_succ1/2).
prim(my_last2/2).
prim(my_succ3/2).
prim(my_last4/2).
prim(my_last5/2).
prim(my_head6/2).
prim(my_tail7/2).
prim(my_max_list8/2).
prim(my_len9/2).
prim(my_succ10/2).
prim(my_last11/2).
prim(my_len12/2).
prim(my_pred13/2).
prim(my_reverse14/2).
prim(my_tail15/2).
prim(my_reverse16/2).
prim(my_max_list17/2).
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
p([['d','d','b'],['h','u','p','q'],['j','r','x']],[['d','d'],['h','u','p'],['j','r']]).
p([['u','g','h'],['i','u','l'],['g','h','k']],[['u','g'],['i','u'],['g','h']]).
