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
my_min_list1(A,B):-min_list(A,B).
my_pred2(A,B):-succ(B,A).
my_pred3(A,B):-succ(B,A).
my_max_list4(A,B):-max_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_pred6(A,B):-succ(B,A).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B).
prim(my_len0/2).
prim(my_min_list1/2).
prim(my_pred2/2).
prim(my_pred3/2).
prim(my_max_list4/2).
prim(my_sumlist5/2).
prim(my_pred6/2).
prim(my_reverse7/2).
prim(my_succ8/2).
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
p([['r','q','k','e'],['j','q','t'],['l','n','a','n']],[['r','q','k'],['j','q'],['l','n','a']]).
p([['e','f','m'],['j','v','s']],[['e','f'],['j','v']]).
