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
my_last0(A,B):-last(A,B).
my_max_list1(A,B):-max_list(A,B).
my_succ2(A,B):-succ(A,B).
my_head3([H|_],H).
my_tail4([_|TL],TL).
my_head5([H|_],H).
my_succ6(A,B):-succ(A,B).
my_len7(A,B):-length(A,B).
my_reverse8(A,B):-reverse(A,B).
my_pred9(A,B):-succ(B,A).
my_max_list10(A,B):-max_list(A,B).
my_tail11([_|TL],TL).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_tail14([_|TL],TL).
my_min_list15(A,B):-min_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_succ17(A,B):-succ(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_reverse19(A,B):-reverse(A,B).
my_reverse20(A,B):-reverse(A,B).
prim(my_last0/2).
prim(my_max_list1/2).
prim(my_succ2/2).
prim(my_head3/2).
prim(my_tail4/2).
prim(my_head5/2).
prim(my_succ6/2).
prim(my_len7/2).
prim(my_reverse8/2).
prim(my_pred9/2).
prim(my_max_list10/2).
prim(my_tail11/2).
prim(my_pred12/2).
prim(my_max_list13/2).
prim(my_tail14/2).
prim(my_min_list15/2).
prim(my_reverse16/2).
prim(my_succ17/2).
prim(my_sumlist18/2).
prim(my_reverse19/2).
prim(my_reverse20/2).
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
p([['k','p','v'],['a','l','n'],['j','f','t','n']],[['k','p'],['a','l'],['j','f','t']]).
p([['y','x','q','i'],['u','q','g'],['k','c','c','n']],[['y','x','q'],['u','q'],['k','c','c']]).
