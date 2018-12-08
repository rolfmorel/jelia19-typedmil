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
my_succ1(A,B):-succ(A,B).
my_head2([H|_],H).
my_head3([H|_],H).
my_last4(A,B):-last(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_last10(A,B):-last(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_tail12([_|TL],TL).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_max_list15(A,B):-max_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_min_list17(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_succ1/2).
prim(my_head2/2).
prim(my_head3/2).
prim(my_last4/2).
prim(my_sumlist5/2).
prim(my_head6/2).
prim(my_tail7/2).
prim(my_head8/2).
prim(my_max_list9/2).
prim(my_last10/2).
prim(my_sumlist11/2).
prim(my_tail12/2).
prim(my_pred13/2).
prim(my_pred14/2).
prim(my_max_list15/2).
prim(my_reverse16/2).
prim(my_min_list17/2).
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
p([['o','a','s','w'],['l','p','x','k'],['u','a','i'],['b','m','p']],[['o','a','s'],['l','p','x'],['u','a'],['b','m']]).
p([['t','w','p','l'],['l','h','y','q'],['q','m','b']],[['t','w','p'],['l','h','y'],['q','m']]).
