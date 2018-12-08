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
my_sumlist1(A,B):-sumlist(A,B).
my_succ2(A,B):-succ(A,B).
my_max_list3(A,B):-max_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_min_list6(A,B):-min_list(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_last9(A,B):-last(A,B).
my_max_list10(A,B):-max_list(A,B).
my_pred11(A,B):-succ(B,A).
my_succ12(A,B):-succ(A,B).
my_tail13([_|TL],TL).
my_tail14([_|TL],TL).
my_tail15([_|TL],TL).
my_head16([H|_],H).
my_min_list17(A,B):-min_list(A,B).
my_max_list18(A,B):-max_list(A,B).
my_pred19(A,B):-succ(B,A).
my_len20(A,B):-length(A,B).
prim(my_last0/2).
prim(my_sumlist1/2).
prim(my_succ2/2).
prim(my_max_list3/2).
prim(my_max_list4/2).
prim(my_min_list5/2).
prim(my_min_list6/2).
prim(my_sumlist7/2).
prim(my_len8/2).
prim(my_last9/2).
prim(my_max_list10/2).
prim(my_pred11/2).
prim(my_succ12/2).
prim(my_tail13/2).
prim(my_tail14/2).
prim(my_tail15/2).
prim(my_head16/2).
prim(my_min_list17/2).
prim(my_max_list18/2).
prim(my_pred19/2).
prim(my_len20/2).
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
p([['m','o','f','v'],['t','g','t','y'],['p','y','v'],['p','o','x']],[['m','o','f'],['t','g','t'],['p','y'],['p','o']]).
p([['u','d','u','a'],['p','a','f']],[['u','d','u'],['p','a']]).
