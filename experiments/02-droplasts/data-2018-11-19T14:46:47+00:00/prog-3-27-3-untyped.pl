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
my_min_list2(A,B):-min_list(A,B).
my_last3(A,B):-last(A,B).
my_pred4(A,B):-succ(B,A).
my_succ5(A,B):-succ(A,B).
my_head6([H|_],H).
my_pred7(A,B):-succ(B,A).
my_len8(A,B):-length(A,B).
my_head9([H|_],H).
my_tail10([_|TL],TL).
my_succ11(A,B):-succ(A,B).
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_len16(A,B):-length(A,B).
my_len17(A,B):-length(A,B).
my_succ18(A,B):-succ(A,B).
my_succ19(A,B):-succ(A,B).
my_last20(A,B):-last(A,B).
my_pred21(A,B):-succ(B,A).
my_tail22([_|TL],TL).
my_min_list23(A,B):-min_list(A,B).
my_tail24([_|TL],TL).
my_reverse25(A,B):-reverse(A,B).
my_len26(A,B):-length(A,B).
prim(my_reverse0/2).
prim(my_succ1/2).
prim(my_min_list2/2).
prim(my_last3/2).
prim(my_pred4/2).
prim(my_succ5/2).
prim(my_head6/2).
prim(my_pred7/2).
prim(my_len8/2).
prim(my_head9/2).
prim(my_tail10/2).
prim(my_succ11/2).
prim(my_head12/2).
prim(my_len13/2).
prim(my_succ14/2).
prim(my_last15/2).
prim(my_len16/2).
prim(my_len17/2).
prim(my_succ18/2).
prim(my_succ19/2).
prim(my_last20/2).
prim(my_pred21/2).
prim(my_tail22/2).
prim(my_min_list23/2).
prim(my_tail24/2).
prim(my_reverse25/2).
prim(my_len26/2).
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
p([['n','c','c'],['f','f','r'],['g','y','c']],[['n','c'],['f','f'],['g','y']]).
p([['w','f','s'],['p','q','m','r']],[['w','f'],['p','q','m']]).
