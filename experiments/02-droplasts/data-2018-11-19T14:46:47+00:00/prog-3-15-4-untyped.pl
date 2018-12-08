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
my_pred1(A,B):-succ(B,A).
my_head2([H|_],H).
my_head3([H|_],H).
my_succ4(A,B):-succ(A,B).
my_pred5(A,B):-succ(B,A).
my_tail6([_|TL],TL).
my_reverse7(A,B):-reverse(A,B).
my_len8(A,B):-length(A,B).
my_last9(A,B):-last(A,B).
my_min_list10(A,B):-min_list(A,B).
my_max_list11(A,B):-max_list(A,B).
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_head14([H|_],H).
prim(my_head0/2).
prim(my_pred1/2).
prim(my_head2/2).
prim(my_head3/2).
prim(my_succ4/2).
prim(my_pred5/2).
prim(my_tail6/2).
prim(my_reverse7/2).
prim(my_len8/2).
prim(my_last9/2).
prim(my_min_list10/2).
prim(my_max_list11/2).
prim(my_head12/2).
prim(my_len13/2).
prim(my_head14/2).
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
p([['c','s','h'],['p','k','y']],[['c','s'],['p','k']]).
p([['a','o','b'],['r','y','e','k']],[['a','o'],['r','y','e']]).
