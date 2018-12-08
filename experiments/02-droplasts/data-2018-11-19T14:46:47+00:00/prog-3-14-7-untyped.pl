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
my_pred0(A,B):-succ(B,A).
my_len1(A,B):-length(A,B).
my_reverse2(A,B):-reverse(A,B).
my_min_list3(A,B):-min_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_head5([H|_],H).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_last8(A,B):-last(A,B).
my_min_list9(A,B):-min_list(A,B).
my_min_list10(A,B):-min_list(A,B).
my_reverse11(A,B):-reverse(A,B).
my_len12(A,B):-length(A,B).
my_reverse13(A,B):-reverse(A,B).
prim(my_pred0/2).
prim(my_len1/2).
prim(my_reverse2/2).
prim(my_min_list3/2).
prim(my_min_list4/2).
prim(my_head5/2).
prim(my_len6/2).
prim(my_sumlist7/2).
prim(my_last8/2).
prim(my_min_list9/2).
prim(my_min_list10/2).
prim(my_reverse11/2).
prim(my_len12/2).
prim(my_reverse13/2).
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
p([['p','l','q','m'],['k','b','a']],[['p','l','q'],['k','b']]).
p([['c','f','w'],['c','v','w'],['b','t','p'],['b','i','y','a']],[['c','f'],['c','v'],['b','t'],['b','i','y']]).
