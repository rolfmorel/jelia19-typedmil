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
my_tail1([_|TL],TL).
my_succ2(A,B):-succ(A,B).
my_min_list3(A,B):-min_list(A,B).
my_succ4(A,B):-succ(A,B).
my_reverse5(A,B):-reverse(A,B).
my_succ6(A,B):-succ(A,B).
my_succ7(A,B):-succ(A,B).
my_tail8([_|TL],TL).
my_last9(A,B):-last(A,B).
my_last10(A,B):-last(A,B).
my_max_list11(A,B):-max_list(A,B).
my_pred12(A,B):-succ(B,A).
my_pred13(A,B):-succ(B,A).
my_tail14([_|TL],TL).
my_head15([H|_],H).
my_min_list16(A,B):-min_list(A,B).
prim(my_reverse0/2).
prim(my_tail1/2).
prim(my_succ2/2).
prim(my_min_list3/2).
prim(my_succ4/2).
prim(my_reverse5/2).
prim(my_succ6/2).
prim(my_succ7/2).
prim(my_tail8/2).
prim(my_last9/2).
prim(my_last10/2).
prim(my_max_list11/2).
prim(my_pred12/2).
prim(my_pred13/2).
prim(my_tail14/2).
prim(my_head15/2).
prim(my_min_list16/2).
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
p([['p','q','b','g'],['f','t','u']],[['p','q','b'],['f','t']]).
p([['u','t','j'],['b','d','k']],[['u','t'],['b','d']]).
