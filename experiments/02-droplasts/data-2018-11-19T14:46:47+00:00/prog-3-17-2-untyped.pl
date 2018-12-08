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
my_len0(A,B):-length(A,B).
my_len1(A,B):-length(A,B).
my_len2(A,B):-length(A,B).
my_tail3([_|TL],TL).
my_last4(A,B):-last(A,B).
my_min_list5(A,B):-min_list(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_tail7([_|TL],TL).
my_reverse8(A,B):-reverse(A,B).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_succ11(A,B):-succ(A,B).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_pred14(A,B):-succ(B,A).
my_succ15(A,B):-succ(A,B).
my_reverse16(A,B):-reverse(A,B).
prim(my_len0/2).
prim(my_len1/2).
prim(my_len2/2).
prim(my_tail3/2).
prim(my_last4/2).
prim(my_min_list5/2).
prim(my_sumlist6/2).
prim(my_tail7/2).
prim(my_reverse8/2).
prim(my_min_list9/2).
prim(my_reverse10/2).
prim(my_succ11/2).
prim(my_reverse12/2).
prim(my_len13/2).
prim(my_pred14/2).
prim(my_succ15/2).
prim(my_reverse16/2).
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
p([['k','a','y'],['g','n','x']],[['k','a'],['g','n']]).
p([['b','f','x','k'],['h','w','n','w'],['l','i','p'],['f','c','f']],[['b','f','x'],['h','w','n'],['l','i'],['f','c']]).
