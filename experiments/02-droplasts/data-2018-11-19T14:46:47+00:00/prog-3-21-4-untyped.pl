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
my_head1([H|_],H).
my_sumlist2(A,B):-sumlist(A,B).
my_succ3(A,B):-succ(A,B).
my_tail4([_|TL],TL).
my_succ5(A,B):-succ(A,B).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A).
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_succ11(A,B):-succ(A,B).
my_head12([H|_],H).
my_reverse13(A,B):-reverse(A,B).
my_succ14(A,B):-succ(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_min_list16(A,B):-min_list(A,B).
my_last17(A,B):-last(A,B).
my_max_list18(A,B):-max_list(A,B).
my_succ19(A,B):-succ(A,B).
my_sumlist20(A,B):-sumlist(A,B).
prim(my_pred0/2).
prim(my_head1/2).
prim(my_sumlist2/2).
prim(my_succ3/2).
prim(my_tail4/2).
prim(my_succ5/2).
prim(my_len6/2).
prim(my_pred7/2).
prim(my_head8/2).
prim(my_max_list9/2).
prim(my_sumlist10/2).
prim(my_succ11/2).
prim(my_head12/2).
prim(my_reverse13/2).
prim(my_succ14/2).
prim(my_sumlist15/2).
prim(my_min_list16/2).
prim(my_last17/2).
prim(my_max_list18/2).
prim(my_succ19/2).
prim(my_sumlist20/2).
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
p([['f','s','j'],['k','l','a','x'],['j','u','r','n']],[['f','s'],['k','l','a'],['j','u','r']]).
p([['t','w','m','n'],['d','o','u'],['c','o','i','h'],['g','u','s']],[['t','w','m'],['d','o'],['c','o','i'],['g','u']]).
