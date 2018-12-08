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
my_succ1(A,B):-succ(A,B).
my_len2(A,B):-length(A,B).
my_succ3(A,B):-succ(A,B).
my_tail4([_|TL],TL).
my_reverse5(A,B):-reverse(A,B).
my_succ6(A,B):-succ(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_reverse9(A,B):-reverse(A,B).
my_min_list10(A,B):-min_list(A,B).
my_last11(A,B):-last(A,B).
my_max_list12(A,B):-max_list(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_last16(A,B):-last(A,B).
my_tail17([_|TL],TL).
my_len18(A,B):-length(A,B).
my_tail19([_|TL],TL).
my_reverse20(A,B):-reverse(A,B).
my_head21([H|_],H).
my_tail22([_|TL],TL).
prim(my_reverse0/2).
prim(my_succ1/2).
prim(my_len2/2).
prim(my_succ3/2).
prim(my_tail4/2).
prim(my_reverse5/2).
prim(my_succ6/2).
prim(my_sumlist7/2).
prim(my_len8/2).
prim(my_reverse9/2).
prim(my_min_list10/2).
prim(my_last11/2).
prim(my_max_list12/2).
prim(my_sumlist13/2).
prim(my_max_list14/2).
prim(my_sumlist15/2).
prim(my_last16/2).
prim(my_tail17/2).
prim(my_len18/2).
prim(my_tail19/2).
prim(my_reverse20/2).
prim(my_head21/2).
prim(my_tail22/2).
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
p([['p','k','e','f'],['i','l','e'],['y','e','m','m']],[['p','k','e'],['i','l'],['y','e','m']]).
p([['m','b','f','v'],['q','c','i','a'],['m','p','d','q']],[['m','b','f'],['q','c','i'],['m','p','d']]).
