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
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_succ3(A,B):-succ(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_reverse5(A,B):-reverse(A,B).
my_len6(A,B):-length(A,B).
my_succ7(A,B):-succ(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A).
my_pred10(A,B):-succ(B,A).
my_tail11([_|TL],TL).
my_last12(A,B):-last(A,B).
my_head13([H|_],H).
my_last14(A,B):-last(A,B).
my_tail15([_|TL],TL).
my_pred16(A,B):-succ(B,A).
my_pred17(A,B):-succ(B,A).
my_reverse18(A,B):-reverse(A,B).
prim(my_last0/2).
prim(my_tail1/2).
prim(my_min_list2/2).
prim(my_succ3/2).
prim(my_sumlist4/2).
prim(my_reverse5/2).
prim(my_len6/2).
prim(my_succ7/2).
prim(my_len8/2).
prim(my_pred9/2).
prim(my_pred10/2).
prim(my_tail11/2).
prim(my_last12/2).
prim(my_head13/2).
prim(my_last14/2).
prim(my_tail15/2).
prim(my_pred16/2).
prim(my_pred17/2).
prim(my_reverse18/2).
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
p([['p','b','o'],['p','t','q','o'],['p','j','x','k']],[['p','b'],['p','t','q'],['p','j','x']]).
p([['w','m','w'],['m','e','o','p'],['y','o','g'],['d','y','x']],[['w','m'],['m','e','o'],['y','o'],['d','y']]).
