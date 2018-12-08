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
my_pred1(A,B):-succ(B,A).
my_len2(A,B):-length(A,B).
my_min_list3(A,B):-min_list(A,B).
my_len4(A,B):-length(A,B).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_succ7(A,B):-succ(A,B).
my_reverse8(A,B):-reverse(A,B).
my_head9([H|_],H).
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
my_head12([H|_],H).
my_tail13([_|TL],TL).
my_head14([H|_],H).
my_last15(A,B):-last(A,B).
my_last16(A,B):-last(A,B).
my_last17(A,B):-last(A,B).
my_succ18(A,B):-succ(A,B).
my_pred19(A,B):-succ(B,A).
my_head20([H|_],H).
my_reverse21(A,B):-reverse(A,B).
my_len22(A,B):-length(A,B).
prim(my_last0/2).
prim(my_pred1/2).
prim(my_len2/2).
prim(my_min_list3/2).
prim(my_len4/2).
prim(my_succ5/2).
prim(my_head6/2).
prim(my_succ7/2).
prim(my_reverse8/2).
prim(my_head9/2).
prim(my_sumlist10/2).
prim(my_tail11/2).
prim(my_head12/2).
prim(my_tail13/2).
prim(my_head14/2).
prim(my_last15/2).
prim(my_last16/2).
prim(my_last17/2).
prim(my_succ18/2).
prim(my_pred19/2).
prim(my_head20/2).
prim(my_reverse21/2).
prim(my_len22/2).
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
p([['q','l','a'],['o','e','f','e']],[['q','l'],['o','e','f']]).
p([['p','b','k','g'],['s','g','n'],['i','n','g'],['b','p','v','v']],[['p','b','k'],['s','g'],['i','n'],['b','p','v']]).
