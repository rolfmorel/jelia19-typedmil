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
my_reverse1(A,B):-reverse(A,B).
my_head2([H|_],H).
my_min_list3(A,B):-min_list(A,B).
my_last4(A,B):-last(A,B).
my_tail5([_|TL],TL).
my_tail6([_|TL],TL).
my_succ7(A,B):-succ(A,B).
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_len12(A,B):-length(A,B).
my_pred13(A,B):-succ(B,A).
my_last14(A,B):-last(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_last17(A,B):-last(A,B).
my_reverse18(A,B):-reverse(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_tail20([_|TL],TL).
prim(my_last0/2).
prim(my_reverse1/2).
prim(my_head2/2).
prim(my_min_list3/2).
prim(my_last4/2).
prim(my_tail5/2).
prim(my_tail6/2).
prim(my_succ7/2).
prim(my_head8/2).
prim(my_max_list9/2).
prim(my_min_list10/2).
prim(my_sumlist11/2).
prim(my_len12/2).
prim(my_pred13/2).
prim(my_last14/2).
prim(my_sumlist15/2).
prim(my_sumlist16/2).
prim(my_last17/2).
prim(my_reverse18/2).
prim(my_sumlist19/2).
prim(my_tail20/2).
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
p([['n','s','b','g'],['q','x','u','a'],['t','n','g']],[['n','s','b'],['q','x','u'],['t','n']]).
p([['f','t','c'],['e','o','m'],['r','r','k','o'],['g','t','x','t']],[['f','t'],['e','o'],['r','r','k'],['g','t','x']]).
