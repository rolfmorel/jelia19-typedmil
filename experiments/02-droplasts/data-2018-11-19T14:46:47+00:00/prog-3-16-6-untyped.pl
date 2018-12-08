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
my_min_list1(A,B):-min_list(A,B).
my_min_list2(A,B):-min_list(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_last4(A,B):-last(A,B).
my_min_list5(A,B):-min_list(A,B).
my_succ6(A,B):-succ(A,B).
my_succ7(A,B):-succ(A,B).
my_min_list8(A,B):-min_list(A,B).
my_last9(A,B):-last(A,B).
my_succ10(A,B):-succ(A,B).
my_len11(A,B):-length(A,B).
my_max_list12(A,B):-max_list(A,B).
my_max_list13(A,B):-max_list(A,B).
my_head14([H|_],H).
my_tail15([_|TL],TL).
prim(my_succ0/2).
prim(my_min_list1/2).
prim(my_min_list2/2).
prim(my_sumlist3/2).
prim(my_last4/2).
prim(my_min_list5/2).
prim(my_succ6/2).
prim(my_succ7/2).
prim(my_min_list8/2).
prim(my_last9/2).
prim(my_succ10/2).
prim(my_len11/2).
prim(my_max_list12/2).
prim(my_max_list13/2).
prim(my_head14/2).
prim(my_tail15/2).
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
p([['l','a','x'],['k','j','w'],['d','c','b','v']],[['l','a'],['k','j'],['d','c','b']]).
p([['c','h','n'],['x','k','u'],['j','w','y'],['a','k','u']],[['c','h'],['x','k'],['j','w'],['a','k']]).
