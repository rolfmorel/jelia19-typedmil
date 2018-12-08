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
my_sumlist0(A,B):-sumlist(A,B).
my_tail1([_|TL],TL).
my_last2(A,B):-last(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_head5([H|_],H).
my_tail6([_|TL],TL).
my_pred7(A,B):-succ(B,A).
my_tail8([_|TL],TL).
my_tail9([_|TL],TL).
my_tail10([_|TL],TL).
my_max_list11(A,B):-max_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_last13(A,B):-last(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_reverse15(A,B):-reverse(A,B).
my_head16([H|_],H).
my_reverse17(A,B):-reverse(A,B).
my_pred18(A,B):-succ(B,A).
my_last19(A,B):-last(A,B).
prim(my_sumlist0/2).
prim(my_tail1/2).
prim(my_last2/2).
prim(my_sumlist3/2).
prim(my_sumlist4/2).
prim(my_head5/2).
prim(my_tail6/2).
prim(my_pred7/2).
prim(my_tail8/2).
prim(my_tail9/2).
prim(my_tail10/2).
prim(my_max_list11/2).
prim(my_reverse12/2).
prim(my_last13/2).
prim(my_sumlist14/2).
prim(my_reverse15/2).
prim(my_head16/2).
prim(my_reverse17/2).
prim(my_pred18/2).
prim(my_last19/2).
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
p([['k','e','n'],['r','f','g','p']],[['k','e'],['r','f','g']]).
p([['q','r','l','x'],['a','k','j','k']],[['q','r','l'],['a','k','j']]).
