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
my_tail1([_|TL],TL).
my_head2([H|_],H).
my_tail3([_|TL],TL).
my_max_list4(A,B):-max_list(A,B).
my_tail5([_|TL],TL).
my_head6([H|_],H).
my_sumlist7(A,B):-sumlist(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_tail9([_|TL],TL).
my_min_list10(A,B):-min_list(A,B).
my_len11(A,B):-length(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_reverse13(A,B):-reverse(A,B).
my_max_list14(A,B):-max_list(A,B).
my_last15(A,B):-last(A,B).
my_max_list16(A,B):-max_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_len18(A,B):-length(A,B).
my_pred19(A,B):-succ(B,A).
prim(my_len0/2).
prim(my_tail1/2).
prim(my_head2/2).
prim(my_tail3/2).
prim(my_max_list4/2).
prim(my_tail5/2).
prim(my_head6/2).
prim(my_sumlist7/2).
prim(my_sumlist8/2).
prim(my_tail9/2).
prim(my_min_list10/2).
prim(my_len11/2).
prim(my_sumlist12/2).
prim(my_reverse13/2).
prim(my_max_list14/2).
prim(my_last15/2).
prim(my_max_list16/2).
prim(my_sumlist17/2).
prim(my_len18/2).
prim(my_pred19/2).
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
p([['m','s','i','y'],['l','v','m'],['i','a','f','f']],[['m','s','i'],['l','v'],['i','a','f']]).
p([['u','e','c'],['o','h','s']],[['u','e'],['o','h']]).
