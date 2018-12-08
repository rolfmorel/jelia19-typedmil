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
my_succ1(A,B):-succ(A,B).
my_head2([H|_],H).
my_last3(A,B):-last(A,B).
my_max_list4(A,B):-max_list(A,B).
my_tail5([_|TL],TL).
my_last6(A,B):-last(A,B).
my_pred7(A,B):-succ(B,A).
my_last8(A,B):-last(A,B).
my_tail9([_|TL],TL).
my_len10(A,B):-length(A,B).
my_last11(A,B):-last(A,B).
my_tail12([_|TL],TL).
my_head13([H|_],H).
my_tail14([_|TL],TL).
my_head15([H|_],H).
my_max_list16(A,B):-max_list(A,B).
my_min_list17(A,B):-min_list(A,B).
my_tail18([_|TL],TL).
my_last19(A,B):-last(A,B).
my_last20(A,B):-last(A,B).
my_succ21(A,B):-succ(A,B).
my_succ22(A,B):-succ(A,B).
prim(my_reverse0/2).
prim(my_succ1/2).
prim(my_head2/2).
prim(my_last3/2).
prim(my_max_list4/2).
prim(my_tail5/2).
prim(my_last6/2).
prim(my_pred7/2).
prim(my_last8/2).
prim(my_tail9/2).
prim(my_len10/2).
prim(my_last11/2).
prim(my_tail12/2).
prim(my_head13/2).
prim(my_tail14/2).
prim(my_head15/2).
prim(my_max_list16/2).
prim(my_min_list17/2).
prim(my_tail18/2).
prim(my_last19/2).
prim(my_last20/2).
prim(my_succ21/2).
prim(my_succ22/2).
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
p([['y','a','x'],['y','k','a'],['x','r','x','a'],['y','t','e']],[['y','a'],['y','k'],['x','r','x'],['y','t']]).
p([['d','s','v','y'],['o','e','k'],['k','n','l'],['a','m','s']],[['d','s','v'],['o','e'],['k','n'],['a','m']]).
