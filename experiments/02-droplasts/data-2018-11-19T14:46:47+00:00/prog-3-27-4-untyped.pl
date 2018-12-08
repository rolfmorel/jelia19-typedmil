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
my_succ1(A,B):-succ(A,B).
my_last2(A,B):-last(A,B).
my_reverse3(A,B):-reverse(A,B).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_min_list6(A,B):-min_list(A,B).
my_succ7(A,B):-succ(A,B).
my_pred8(A,B):-succ(B,A).
my_reverse9(A,B):-reverse(A,B).
my_max_list10(A,B):-max_list(A,B).
my_max_list11(A,B):-max_list(A,B).
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_last17(A,B):-last(A,B).
my_max_list18(A,B):-max_list(A,B).
my_pred19(A,B):-succ(B,A).
my_len20(A,B):-length(A,B).
my_succ21(A,B):-succ(A,B).
my_len22(A,B):-length(A,B).
my_len23(A,B):-length(A,B).
my_min_list24(A,B):-min_list(A,B).
my_min_list25(A,B):-min_list(A,B).
my_min_list26(A,B):-min_list(A,B).
prim(my_last0/2).
prim(my_succ1/2).
prim(my_last2/2).
prim(my_reverse3/2).
prim(my_max_list4/2).
prim(my_head5/2).
prim(my_min_list6/2).
prim(my_succ7/2).
prim(my_pred8/2).
prim(my_reverse9/2).
prim(my_max_list10/2).
prim(my_max_list11/2).
prim(my_head12/2).
prim(my_len13/2).
prim(my_len14/2).
prim(my_last15/2).
prim(my_sumlist16/2).
prim(my_last17/2).
prim(my_max_list18/2).
prim(my_pred19/2).
prim(my_len20/2).
prim(my_succ21/2).
prim(my_len22/2).
prim(my_len23/2).
prim(my_min_list24/2).
prim(my_min_list25/2).
prim(my_min_list26/2).
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
p([['x','w','r'],['i','h','q','i'],['r','u','n','p']],[['x','w'],['i','h','q'],['r','u','n']]).
p([['w','w','h','v'],['j','b','x'],['j','y','p'],['y','t','e','x']],[['w','w','h'],['j','b'],['j','y'],['y','t','e']]).
