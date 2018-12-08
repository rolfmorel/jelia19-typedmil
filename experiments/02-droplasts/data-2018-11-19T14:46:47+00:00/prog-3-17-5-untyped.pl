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
my_len1(A,B):-length(A,B).
my_len2(A,B):-length(A,B).
my_min_list3(A,B):-min_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_pred5(A,B):-succ(B,A).
my_reverse6(A,B):-reverse(A,B).
my_len7(A,B):-length(A,B).
my_succ8(A,B):-succ(A,B).
my_len9(A,B):-length(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_len11(A,B):-length(A,B).
my_last12(A,B):-last(A,B).
my_tail13([_|TL],TL).
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_sumlist16(A,B):-sumlist(A,B).
prim(my_head0/2).
prim(my_len1/2).
prim(my_len2/2).
prim(my_min_list3/2).
prim(my_min_list4/2).
prim(my_pred5/2).
prim(my_reverse6/2).
prim(my_len7/2).
prim(my_succ8/2).
prim(my_len9/2).
prim(my_sumlist10/2).
prim(my_len11/2).
prim(my_last12/2).
prim(my_tail13/2).
prim(my_len14/2).
prim(my_last15/2).
prim(my_sumlist16/2).
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
p([['w','e','q','u'],['q','k','f','i'],['c','h','k'],['w','u','p']],[['w','e','q'],['q','k','f'],['c','h'],['w','u']]).
p([['e','s','h'],['a','v','k','m']],[['e','s'],['a','v','k']]).
