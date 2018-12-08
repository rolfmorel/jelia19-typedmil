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
my_max_list0(A,B):-max_list(A,B).
my_succ1(A,B):-succ(A,B).
my_max_list2(A,B):-max_list(A,B).
my_len3(A,B):-length(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_len5(A,B):-length(A,B).
my_max_list6(A,B):-max_list(A,B).
my_pred7(A,B):-succ(B,A).
my_reverse8(A,B):-reverse(A,B).
my_reverse9(A,B):-reverse(A,B).
my_max_list10(A,B):-max_list(A,B).
prim(my_max_list0/2).
prim(my_succ1/2).
prim(my_max_list2/2).
prim(my_len3/2).
prim(my_sumlist4/2).
prim(my_len5/2).
prim(my_max_list6/2).
prim(my_pred7/2).
prim(my_reverse8/2).
prim(my_reverse9/2).
prim(my_max_list10/2).
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
p([['d','y','h'],['n','s','b']],[['d','y'],['n','s']]).
p([['g','p','t'],['d','q','w','x']],[['g','p'],['d','q','w']]).
