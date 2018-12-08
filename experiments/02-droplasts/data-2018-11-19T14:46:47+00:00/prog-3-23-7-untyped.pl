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
my_sumlist1(A,B):-sumlist(A,B).
my_succ2(A,B):-succ(A,B).
my_pred3(A,B):-succ(B,A).
my_len4(A,B):-length(A,B).
my_pred5(A,B):-succ(B,A).
my_succ6(A,B):-succ(A,B).
my_head7([H|_],H).
my_reverse8(A,B):-reverse(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_tail11([_|TL],TL).
my_max_list12(A,B):-max_list(A,B).
my_len13(A,B):-length(A,B).
my_pred14(A,B):-succ(B,A).
my_reverse15(A,B):-reverse(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_pred19(A,B):-succ(B,A).
my_min_list20(A,B):-min_list(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_tail22([_|TL],TL).
prim(my_len0/2).
prim(my_sumlist1/2).
prim(my_succ2/2).
prim(my_pred3/2).
prim(my_len4/2).
prim(my_pred5/2).
prim(my_succ6/2).
prim(my_head7/2).
prim(my_reverse8/2).
prim(my_last9/2).
prim(my_head10/2).
prim(my_tail11/2).
prim(my_max_list12/2).
prim(my_len13/2).
prim(my_pred14/2).
prim(my_reverse15/2).
prim(my_sumlist16/2).
prim(my_head17/2).
prim(my_sumlist18/2).
prim(my_pred19/2).
prim(my_min_list20/2).
prim(my_sumlist21/2).
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
p([['g','r','l'],['o','b','c','p']],[['g','r'],['o','b','c']]).
p([['b','r','v'],['n','p','p'],['p','v','f','u'],['i','j','f','w']],[['b','r'],['n','p'],['p','v','f'],['i','j','f']]).
