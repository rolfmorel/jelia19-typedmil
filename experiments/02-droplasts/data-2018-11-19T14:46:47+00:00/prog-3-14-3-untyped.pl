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
my_min_list1(A,B):-min_list(A,B).
my_succ2(A,B):-succ(A,B).
my_pred3(A,B):-succ(B,A).
my_head4([H|_],H).
my_reverse5(A,B):-reverse(A,B).
my_max_list6(A,B):-max_list(A,B).
my_min_list7(A,B):-min_list(A,B).
my_head8([H|_],H).
my_tail9([_|TL],TL).
my_succ10(A,B):-succ(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
prim(my_len0/2).
prim(my_min_list1/2).
prim(my_succ2/2).
prim(my_pred3/2).
prim(my_head4/2).
prim(my_reverse5/2).
prim(my_max_list6/2).
prim(my_min_list7/2).
prim(my_head8/2).
prim(my_tail9/2).
prim(my_succ10/2).
prim(my_sumlist11/2).
prim(my_reverse12/2).
prim(my_len13/2).
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
p([['g','r','k'],['h','x','b','s'],['d','l','j'],['w','s','o','d']],[['g','r'],['h','x','b'],['d','l'],['w','s','o']]).
p([['j','d','x','x'],['p','g','e','e']],[['j','d','x'],['p','g','e']]).
