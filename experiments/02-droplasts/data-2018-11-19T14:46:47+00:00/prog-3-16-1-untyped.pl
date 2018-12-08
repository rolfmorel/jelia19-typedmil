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
my_min_list0(A,B):-min_list(A,B).
my_len1(A,B):-length(A,B).
my_head2([H|_],H).
my_head3([H|_],H).
my_head4([H|_],H).
my_len5(A,B):-length(A,B).
my_head6([H|_],H).
my_last7(A,B):-last(A,B).
my_max_list8(A,B):-max_list(A,B).
my_head9([H|_],H).
my_tail10([_|TL],TL).
my_tail11([_|TL],TL).
my_pred12(A,B):-succ(B,A).
my_len13(A,B):-length(A,B).
my_head14([H|_],H).
my_pred15(A,B):-succ(B,A).
prim(my_min_list0/2).
prim(my_len1/2).
prim(my_head2/2).
prim(my_head3/2).
prim(my_head4/2).
prim(my_len5/2).
prim(my_head6/2).
prim(my_last7/2).
prim(my_max_list8/2).
prim(my_head9/2).
prim(my_tail10/2).
prim(my_tail11/2).
prim(my_pred12/2).
prim(my_len13/2).
prim(my_head14/2).
prim(my_pred15/2).
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
p([['u','l','u'],['c','i','d','i'],['n','e','u','m'],['l','w','g']],[['u','l'],['c','i','d'],['n','e','u'],['l','w']]).
p([['a','e','w'],['o','s','b','f'],['p','n','v'],['s','a','o']],[['a','e'],['o','s','b'],['p','n'],['s','a']]).
