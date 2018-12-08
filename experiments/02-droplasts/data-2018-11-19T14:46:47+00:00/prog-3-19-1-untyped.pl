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
my_succ0(A,B):-succ(A,B).
my_len1(A,B):-length(A,B).
my_last2(A,B):-last(A,B).
my_last3(A,B):-last(A,B).
my_min_list4(A,B):-min_list(A,B).
my_pred5(A,B):-succ(B,A).
my_len6(A,B):-length(A,B).
my_succ7(A,B):-succ(A,B).
my_pred8(A,B):-succ(B,A).
my_max_list9(A,B):-max_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_tail12([_|TL],TL).
my_reverse13(A,B):-reverse(A,B).
my_max_list14(A,B):-max_list(A,B).
my_max_list15(A,B):-max_list(A,B).
my_head16([H|_],H).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
prim(my_succ0/2).
prim(my_len1/2).
prim(my_last2/2).
prim(my_last3/2).
prim(my_min_list4/2).
prim(my_pred5/2).
prim(my_len6/2).
prim(my_succ7/2).
prim(my_pred8/2).
prim(my_max_list9/2).
prim(my_min_list10/2).
prim(my_min_list11/2).
prim(my_tail12/2).
prim(my_reverse13/2).
prim(my_max_list14/2).
prim(my_max_list15/2).
prim(my_head16/2).
prim(my_head17/2).
prim(my_sumlist18/2).
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
p([['y','a','k'],['a','n','x','v'],['x','y','o','g'],['m','q','k']],[['y','a'],['a','n','x'],['x','y','o'],['m','q']]).
p([['m','w','v','t'],['l','u','e','m']],[['m','w','v'],['l','u','e']]).
