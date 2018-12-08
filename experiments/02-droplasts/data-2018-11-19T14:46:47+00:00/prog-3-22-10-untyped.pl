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
my_min_list0(A,B):-min_list(A,B).
my_max_list1(A,B):-max_list(A,B).
my_succ2(A,B):-succ(A,B).
my_reverse3(A,B):-reverse(A,B).
my_reverse4(A,B):-reverse(A,B).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_min_list7(A,B):-min_list(A,B).
my_tail8([_|TL],TL).
my_pred9(A,B):-succ(B,A).
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
my_max_list12(A,B):-max_list(A,B).
my_pred13(A,B):-succ(B,A).
my_reverse14(A,B):-reverse(A,B).
my_reverse15(A,B):-reverse(A,B).
my_succ16(A,B):-succ(A,B).
my_min_list17(A,B):-min_list(A,B).
my_reverse18(A,B):-reverse(A,B).
my_min_list19(A,B):-min_list(A,B).
my_tail20([_|TL],TL).
my_tail21([_|TL],TL).
prim(my_min_list0/2).
prim(my_max_list1/2).
prim(my_succ2/2).
prim(my_reverse3/2).
prim(my_reverse4/2).
prim(my_len5/2).
prim(my_min_list6/2).
prim(my_min_list7/2).
prim(my_tail8/2).
prim(my_pred9/2).
prim(my_sumlist10/2).
prim(my_tail11/2).
prim(my_max_list12/2).
prim(my_pred13/2).
prim(my_reverse14/2).
prim(my_reverse15/2).
prim(my_succ16/2).
prim(my_min_list17/2).
prim(my_reverse18/2).
prim(my_min_list19/2).
prim(my_tail20/2).
prim(my_tail21/2).
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
p([['t','c','j'],['j','c','j'],['w','a','y'],['p','t','r']],[['t','c'],['j','c'],['w','a'],['p','t']]).
p([['p','s','q'],['a','q','d'],['o','l','h'],['v','w','h']],[['p','s'],['a','q'],['o','l'],['v','w']]).
