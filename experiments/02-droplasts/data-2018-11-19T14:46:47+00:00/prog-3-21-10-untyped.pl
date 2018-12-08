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
my_max_list2(A,B):-max_list(A,B).
my_pred3(A,B):-succ(B,A).
my_head4([H|_],H).
my_pred5(A,B):-succ(B,A).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A).
my_succ8(A,B):-succ(A,B).
my_succ9(A,B):-succ(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_pred11(A,B):-succ(B,A).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_len14(A,B):-length(A,B).
my_head15([H|_],H).
my_len16(A,B):-length(A,B).
my_last17(A,B):-last(A,B).
my_len18(A,B):-length(A,B).
my_succ19(A,B):-succ(A,B).
my_succ20(A,B):-succ(A,B).
prim(my_len0/2).
prim(my_tail1/2).
prim(my_max_list2/2).
prim(my_pred3/2).
prim(my_head4/2).
prim(my_pred5/2).
prim(my_len6/2).
prim(my_pred7/2).
prim(my_succ8/2).
prim(my_succ9/2).
prim(my_sumlist10/2).
prim(my_pred11/2).
prim(my_pred12/2).
prim(my_max_list13/2).
prim(my_len14/2).
prim(my_head15/2).
prim(my_len16/2).
prim(my_last17/2).
prim(my_len18/2).
prim(my_succ19/2).
prim(my_succ20/2).
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
p([['s','w','g'],['i','c','p']],[['s','w'],['i','c']]).
p([['f','u','j'],['s','w','b'],['t','t','c','a']],[['f','u'],['s','w'],['t','t','c']]).
