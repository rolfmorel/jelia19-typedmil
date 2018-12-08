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
my_sumlist2(A,B):-sumlist(A,B).
my_pred3(A,B):-succ(B,A).
my_max_list4(A,B):-max_list(A,B).
my_pred5(A,B):-succ(B,A).
my_tail6([_|TL],TL).
my_min_list7(A,B):-min_list(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A).
my_min_list10(A,B):-min_list(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_last13(A,B):-last(A,B).
my_succ14(A,B):-succ(A,B).
prim(my_pred0/2).
prim(my_head1/2).
prim(my_sumlist2/2).
prim(my_pred3/2).
prim(my_max_list4/2).
prim(my_pred5/2).
prim(my_tail6/2).
prim(my_min_list7/2).
prim(my_len8/2).
prim(my_pred9/2).
prim(my_min_list10/2).
prim(my_sumlist11/2).
prim(my_last12/2).
prim(my_last13/2).
prim(my_succ14/2).
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
p([['q','x','f'],['o','u','w']],[['q','x'],['o','u']]).
p([['p','y','e','y'],['c','s','r']],[['p','y','e'],['c','s']]).
