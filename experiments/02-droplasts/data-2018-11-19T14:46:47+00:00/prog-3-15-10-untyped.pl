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
my_sumlist0(A,B):-sumlist(A,B).
my_sumlist1(A,B):-sumlist(A,B).
my_succ2(A,B):-succ(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_succ4(A,B):-succ(A,B).
my_head5([H|_],H).
my_min_list6(A,B):-min_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
my_tail9([_|TL],TL).
my_succ10(A,B):-succ(A,B).
my_reverse11(A,B):-reverse(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_succ13(A,B):-succ(A,B).
my_head14([H|_],H).
prim(my_sumlist0/2).
prim(my_sumlist1/2).
prim(my_succ2/2).
prim(my_sumlist3/2).
prim(my_succ4/2).
prim(my_head5/2).
prim(my_min_list6/2).
prim(my_reverse7/2).
prim(my_last8/2).
prim(my_tail9/2).
prim(my_succ10/2).
prim(my_reverse11/2).
prim(my_sumlist12/2).
prim(my_succ13/2).
prim(my_head14/2).
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
p([['c','o','x'],['n','a','l'],['w','d','u']],[['c','o'],['n','a'],['w','d']]).
p([['r','i','f','y'],['a','i','o','n'],['p','u','s','d'],['f','r','k']],[['r','i','f'],['a','i','o'],['p','u','s'],['f','r']]).
