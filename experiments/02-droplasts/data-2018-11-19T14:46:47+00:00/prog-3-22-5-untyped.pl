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
my_max_list2(A,B):-max_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_reverse4(A,B):-reverse(A,B).
my_max_list5(A,B):-max_list(A,B).
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_len9(A,B):-length(A,B).
my_tail10([_|TL],TL).
my_sumlist11(A,B):-sumlist(A,B).
my_min_list12(A,B):-min_list(A,B).
my_min_list13(A,B):-min_list(A,B).
my_head14([H|_],H).
my_last15(A,B):-last(A,B).
my_head16([H|_],H).
my_min_list17(A,B):-min_list(A,B).
my_max_list18(A,B):-max_list(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_max_list20(A,B):-max_list(A,B).
my_head21([H|_],H).
prim(my_head0/2).
prim(my_len1/2).
prim(my_max_list2/2).
prim(my_reverse3/2).
prim(my_reverse4/2).
prim(my_max_list5/2).
prim(my_len6/2).
prim(my_max_list7/2).
prim(my_sumlist8/2).
prim(my_len9/2).
prim(my_tail10/2).
prim(my_sumlist11/2).
prim(my_min_list12/2).
prim(my_min_list13/2).
prim(my_head14/2).
prim(my_last15/2).
prim(my_head16/2).
prim(my_min_list17/2).
prim(my_max_list18/2).
prim(my_sumlist19/2).
prim(my_max_list20/2).
prim(my_head21/2).
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
p([['k','i','o'],['q','p','t'],['t','n','a'],['g','j','c']],[['k','i'],['q','p'],['t','n'],['g','j']]).
p([['v','w','n'],['q','m','b'],['m','r','e']],[['v','w'],['q','m'],['m','r']]).
