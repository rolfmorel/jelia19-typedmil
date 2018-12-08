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
my_last0(A,B):-last(A,B).
my_min_list1(A,B):-min_list(A,B).
my_last2(A,B):-last(A,B).
my_len3(A,B):-length(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_min_list5(A,B):-min_list(A,B).
my_len6(A,B):-length(A,B).
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A).
my_reverse9(A,B):-reverse(A,B).
my_last10(A,B):-last(A,B).
my_tail11([_|TL],TL).
my_succ12(A,B):-succ(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_max_list14(A,B):-max_list(A,B).
my_max_list15(A,B):-max_list(A,B).
my_tail16([_|TL],TL).
my_head17([H|_],H).
my_min_list18(A,B):-min_list(A,B).
my_len19(A,B):-length(A,B).
my_succ20(A,B):-succ(A,B).
my_pred21(A,B):-succ(B,A).
my_succ22(A,B):-succ(A,B).
my_len23(A,B):-length(A,B).
my_pred24(A,B):-succ(B,A).
prim(my_last0/2).
prim(my_min_list1/2).
prim(my_last2/2).
prim(my_len3/2).
prim(my_sumlist4/2).
prim(my_min_list5/2).
prim(my_len6/2).
prim(my_last7/2).
prim(my_pred8/2).
prim(my_reverse9/2).
prim(my_last10/2).
prim(my_tail11/2).
prim(my_succ12/2).
prim(my_sumlist13/2).
prim(my_max_list14/2).
prim(my_max_list15/2).
prim(my_tail16/2).
prim(my_head17/2).
prim(my_min_list18/2).
prim(my_len19/2).
prim(my_succ20/2).
prim(my_pred21/2).
prim(my_succ22/2).
prim(my_len23/2).
prim(my_pred24/2).
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
p([['w','r','i'],['r','d','k'],['j','h','r']],[['w','r'],['r','d'],['j','h']]).
p([['b','m','n'],['i','v','q','o'],['v','y','a','a']],[['b','m'],['i','v','q'],['v','y','a']]).
