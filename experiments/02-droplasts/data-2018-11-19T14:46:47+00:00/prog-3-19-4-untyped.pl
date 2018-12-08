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
my_len0(A,B):-length(A,B).
my_pred1(A,B):-succ(B,A).
my_sumlist2(A,B):-sumlist(A,B).
my_pred3(A,B):-succ(B,A).
my_min_list4(A,B):-min_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_min_list7(A,B):-min_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_tail9([_|TL],TL).
my_pred10(A,B):-succ(B,A).
my_last11(A,B):-last(A,B).
my_max_list12(A,B):-max_list(A,B).
my_last13(A,B):-last(A,B).
my_max_list14(A,B):-max_list(A,B).
my_tail15([_|TL],TL).
my_sumlist16(A,B):-sumlist(A,B).
my_pred17(A,B):-succ(B,A).
my_head18([H|_],H).
prim(my_len0/2).
prim(my_pred1/2).
prim(my_sumlist2/2).
prim(my_pred3/2).
prim(my_min_list4/2).
prim(my_min_list5/2).
prim(my_tail6/2).
prim(my_min_list7/2).
prim(my_min_list8/2).
prim(my_tail9/2).
prim(my_pred10/2).
prim(my_last11/2).
prim(my_max_list12/2).
prim(my_last13/2).
prim(my_max_list14/2).
prim(my_tail15/2).
prim(my_sumlist16/2).
prim(my_pred17/2).
prim(my_head18/2).
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
p([['q','x','k','v'],['m','b','v'],['j','r','g']],[['q','x','k'],['m','b'],['j','r']]).
p([['d','b','h','t'],['a','g','y'],['g','x','r','y'],['p','q','n','q']],[['d','b','h'],['a','g'],['g','x','r'],['p','q','n']]).
