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
my_reverse0(A,B):-reverse(A,B).
my_pred1(A,B):-succ(B,A).
my_sumlist2(A,B):-sumlist(A,B).
my_max_list3(A,B):-max_list(A,B).
my_tail4([_|TL],TL).
my_succ5(A,B):-succ(A,B).
my_reverse6(A,B):-reverse(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_tail8([_|TL],TL).
my_succ9(A,B):-succ(A,B).
my_len10(A,B):-length(A,B).
my_len11(A,B):-length(A,B).
my_reverse12(A,B):-reverse(A,B).
my_min_list13(A,B):-min_list(A,B).
my_pred14(A,B):-succ(B,A).
my_tail15([_|TL],TL).
my_head16([H|_],H).
prim(my_reverse0/2).
prim(my_pred1/2).
prim(my_sumlist2/2).
prim(my_max_list3/2).
prim(my_tail4/2).
prim(my_succ5/2).
prim(my_reverse6/2).
prim(my_sumlist7/2).
prim(my_tail8/2).
prim(my_succ9/2).
prim(my_len10/2).
prim(my_len11/2).
prim(my_reverse12/2).
prim(my_min_list13/2).
prim(my_pred14/2).
prim(my_tail15/2).
prim(my_head16/2).
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
p([['u','x','u','f'],['o','m','j']],[['u','x','u'],['o','m']]).
p([['t','c','c','c'],['g','n','q'],['p','o','s','i'],['e','o','v']],[['t','c','c'],['g','n'],['p','o','s'],['e','o']]).
