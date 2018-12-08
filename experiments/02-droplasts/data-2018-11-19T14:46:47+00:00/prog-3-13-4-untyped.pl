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
my_head1([H|_],H).
my_sumlist2(A,B):-sumlist(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_min_list4(A,B):-min_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_max_list6(A,B):-max_list(A,B).
my_succ7(A,B):-succ(A,B).
my_min_list8(A,B):-min_list(A,B).
my_pred9(A,B):-succ(B,A).
my_max_list10(A,B):-max_list(A,B).
my_succ11(A,B):-succ(A,B).
my_pred12(A,B):-succ(B,A).
prim(my_last0/2).
prim(my_head1/2).
prim(my_sumlist2/2).
prim(my_sumlist3/2).
prim(my_min_list4/2).
prim(my_min_list5/2).
prim(my_max_list6/2).
prim(my_succ7/2).
prim(my_min_list8/2).
prim(my_pred9/2).
prim(my_max_list10/2).
prim(my_succ11/2).
prim(my_pred12/2).
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
p([['r','t','n','p'],['q','h','n']],[['r','t','n'],['q','h']]).
p([['c','o','e','i'],['v','l','q','h'],['b','h','f']],[['c','o','e'],['v','l','q'],['b','h']]).
