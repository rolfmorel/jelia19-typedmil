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
my_tail1([_|TL],TL).
my_succ2(A,B):-succ(A,B).
my_max_list3(A,B):-max_list(A,B).
my_last4(A,B):-last(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_pred6(A,B):-succ(B,A).
my_pred7(A,B):-succ(B,A).
my_succ8(A,B):-succ(A,B).
my_head9([H|_],H).
my_len10(A,B):-length(A,B).
my_last11(A,B):-last(A,B).
my_last12(A,B):-last(A,B).
my_len13(A,B):-length(A,B).
my_sumlist14(A,B):-sumlist(A,B).
prim(my_len0/2).
prim(my_tail1/2).
prim(my_succ2/2).
prim(my_max_list3/2).
prim(my_last4/2).
prim(my_sumlist5/2).
prim(my_pred6/2).
prim(my_pred7/2).
prim(my_succ8/2).
prim(my_head9/2).
prim(my_len10/2).
prim(my_last11/2).
prim(my_last12/2).
prim(my_len13/2).
prim(my_sumlist14/2).
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
p([['r','j','q'],['h','o','l','u'],['e','b','o','m']],[['r','j'],['h','o','l'],['e','b','o']]).
p([['x','t','b','i'],['e','d','f']],[['x','t','b'],['e','d']]).
