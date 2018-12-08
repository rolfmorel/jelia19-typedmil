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
my_last1(A,B):-last(A,B).
my_max_list2(A,B):-max_list(A,B).
my_tail3([_|TL],TL).
my_head4([H|_],H).
my_tail5([_|TL],TL).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_head9([H|_],H).
my_succ10(A,B):-succ(A,B).
my_tail11([_|TL],TL).
my_min_list12(A,B):-min_list(A,B).
my_reverse13(A,B):-reverse(A,B).
my_tail14([_|TL],TL).
my_reverse15(A,B):-reverse(A,B).
my_last16(A,B):-last(A,B).
my_tail17([_|TL],TL).
my_max_list18(A,B):-max_list(A,B).
my_reverse19(A,B):-reverse(A,B).
prim(my_reverse0/2).
prim(my_last1/2).
prim(my_max_list2/2).
prim(my_tail3/2).
prim(my_head4/2).
prim(my_tail5/2).
prim(my_len6/2).
prim(my_sumlist7/2).
prim(my_sumlist8/2).
prim(my_head9/2).
prim(my_succ10/2).
prim(my_tail11/2).
prim(my_min_list12/2).
prim(my_reverse13/2).
prim(my_tail14/2).
prim(my_reverse15/2).
prim(my_last16/2).
prim(my_tail17/2).
prim(my_max_list18/2).
prim(my_reverse19/2).
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
p([['y','j','i'],['s','y','y']],[['y','j'],['s','y']]).
p([['k','d','w','n'],['i','x','a']],[['k','d','w'],['i','x']]).
