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
my_last1(A,B):-last(A,B).
my_pred2(A,B):-succ(B,A).
my_sumlist3(A,B):-sumlist(A,B).
my_min_list4(A,B):-min_list(A,B).
my_succ5(A,B):-succ(A,B).
my_pred6(A,B):-succ(B,A).
my_head7([H|_],H).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B).
my_tail10([_|TL],TL).
my_len11(A,B):-length(A,B).
my_last12(A,B):-last(A,B).
prim(my_head0/2).
prim(my_last1/2).
prim(my_pred2/2).
prim(my_sumlist3/2).
prim(my_min_list4/2).
prim(my_succ5/2).
prim(my_pred6/2).
prim(my_head7/2).
prim(my_len8/2).
prim(my_succ9/2).
prim(my_tail10/2).
prim(my_len11/2).
prim(my_last12/2).
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
p([['g','k','u','o'],['h','p','n'],['u','c','e','r'],['t','v','w','y']],[['g','k','u'],['h','p'],['u','c','e'],['t','v','w']]).
p([['l','n','h','m'],['w','m','g']],[['l','n','h'],['w','m']]).
