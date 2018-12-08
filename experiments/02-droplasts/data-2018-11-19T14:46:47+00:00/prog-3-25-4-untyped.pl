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
my_len1(A,B):-length(A,B).
my_len2(A,B):-length(A,B).
my_reverse3(A,B):-reverse(A,B).
my_len4(A,B):-length(A,B).
my_len5(A,B):-length(A,B).
my_pred6(A,B):-succ(B,A).
my_succ7(A,B):-succ(A,B).
my_pred8(A,B):-succ(B,A).
my_succ9(A,B):-succ(A,B).
my_pred10(A,B):-succ(B,A).
my_last11(A,B):-last(A,B).
my_min_list12(A,B):-min_list(A,B).
my_len13(A,B):-length(A,B).
my_min_list14(A,B):-min_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_last16(A,B):-last(A,B).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_head19([H|_],H).
my_len20(A,B):-length(A,B).
my_reverse21(A,B):-reverse(A,B).
my_head22([H|_],H).
my_succ23(A,B):-succ(A,B).
my_sumlist24(A,B):-sumlist(A,B).
prim(my_reverse0/2).
prim(my_len1/2).
prim(my_len2/2).
prim(my_reverse3/2).
prim(my_len4/2).
prim(my_len5/2).
prim(my_pred6/2).
prim(my_succ7/2).
prim(my_pred8/2).
prim(my_succ9/2).
prim(my_pred10/2).
prim(my_last11/2).
prim(my_min_list12/2).
prim(my_len13/2).
prim(my_min_list14/2).
prim(my_min_list15/2).
prim(my_last16/2).
prim(my_head17/2).
prim(my_sumlist18/2).
prim(my_head19/2).
prim(my_len20/2).
prim(my_reverse21/2).
prim(my_head22/2).
prim(my_succ23/2).
prim(my_sumlist24/2).
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
p([['b','x','v'],['h','e','o'],['s','c','p','p']],[['b','x'],['h','e'],['s','c','p']]).
p([['o','d','c','r'],['q','g','f','b']],[['o','d','c'],['q','g','f']]).
