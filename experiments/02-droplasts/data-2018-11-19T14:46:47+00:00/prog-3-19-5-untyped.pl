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
my_head2([H|_],H).
my_max_list3(A,B):-max_list(A,B).
my_succ4(A,B):-succ(A,B).
my_pred5(A,B):-succ(B,A).
my_len6(A,B):-length(A,B).
my_reverse7(A,B):-reverse(A,B).
my_pred8(A,B):-succ(B,A).
my_min_list9(A,B):-min_list(A,B).
my_head10([H|_],H).
my_sumlist11(A,B):-sumlist(A,B).
my_len12(A,B):-length(A,B).
my_head13([H|_],H).
my_head14([H|_],H).
my_tail15([_|TL],TL).
my_len16(A,B):-length(A,B).
my_reverse17(A,B):-reverse(A,B).
my_pred18(A,B):-succ(B,A).
prim(my_head0/2).
prim(my_min_list1/2).
prim(my_head2/2).
prim(my_max_list3/2).
prim(my_succ4/2).
prim(my_pred5/2).
prim(my_len6/2).
prim(my_reverse7/2).
prim(my_pred8/2).
prim(my_min_list9/2).
prim(my_head10/2).
prim(my_sumlist11/2).
prim(my_len12/2).
prim(my_head13/2).
prim(my_head14/2).
prim(my_tail15/2).
prim(my_len16/2).
prim(my_reverse17/2).
prim(my_pred18/2).
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
p([['u','m','p','q'],['i','f','k']],[['u','m','p'],['i','f']]).
p([['b','y','x'],['s','o','j'],['u','g','i'],['h','q','r']],[['b','y'],['s','o'],['u','g'],['h','q']]).
