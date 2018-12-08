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
my_tail0([_|TL],TL).
my_reverse1(A,B):-reverse(A,B).
my_pred2(A,B):-succ(B,A).
my_reverse3(A,B):-reverse(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_min_list5(A,B):-min_list(A,B).
my_max_list6(A,B):-max_list(A,B).
my_succ7(A,B):-succ(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_tail9([_|TL],TL).
my_pred10(A,B):-succ(B,A).
my_max_list11(A,B):-max_list(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_tail13([_|TL],TL).
my_head14([H|_],H).
my_last15(A,B):-last(A,B).
my_succ16(A,B):-succ(A,B).
my_head17([H|_],H).
my_reverse18(A,B):-reverse(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_head20([H|_],H).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_pred2/2).
prim(my_reverse3/2).
prim(my_sumlist4/2).
prim(my_min_list5/2).
prim(my_max_list6/2).
prim(my_succ7/2).
prim(my_sumlist8/2).
prim(my_tail9/2).
prim(my_pred10/2).
prim(my_max_list11/2).
prim(my_sumlist12/2).
prim(my_tail13/2).
prim(my_head14/2).
prim(my_last15/2).
prim(my_succ16/2).
prim(my_head17/2).
prim(my_reverse18/2).
prim(my_sumlist19/2).
prim(my_head20/2).
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
p([['f','i','i','d'],['w','r','u'],['c','e','e']],[['f','i','i'],['w','r'],['c','e']]).
p([['p','u','a','q'],['b','d','d','i'],['w','g','p'],['j','n','o','h']],[['p','u','a'],['b','d','d'],['w','g'],['j','n','o']]).
