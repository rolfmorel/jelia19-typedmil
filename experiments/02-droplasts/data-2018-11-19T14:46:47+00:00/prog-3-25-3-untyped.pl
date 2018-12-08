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
my_min_list1(A,B):-min_list(A,B).
my_max_list2(A,B):-max_list(A,B).
my_len3(A,B):-length(A,B).
my_head4([H|_],H).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B).
my_succ7(A,B):-succ(A,B).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B).
my_tail10([_|TL],TL).
my_pred11(A,B):-succ(B,A).
my_sumlist12(A,B):-sumlist(A,B).
my_succ13(A,B):-succ(A,B).
my_succ14(A,B):-succ(A,B).
my_succ15(A,B):-succ(A,B).
my_reverse16(A,B):-reverse(A,B).
my_max_list17(A,B):-max_list(A,B).
my_succ18(A,B):-succ(A,B).
my_tail19([_|TL],TL).
my_pred20(A,B):-succ(B,A).
my_sumlist21(A,B):-sumlist(A,B).
my_max_list22(A,B):-max_list(A,B).
my_pred23(A,B):-succ(B,A).
my_last24(A,B):-last(A,B).
prim(my_min_list0/2).
prim(my_min_list1/2).
prim(my_max_list2/2).
prim(my_len3/2).
prim(my_head4/2).
prim(my_sumlist5/2).
prim(my_succ6/2).
prim(my_succ7/2).
prim(my_len8/2).
prim(my_succ9/2).
prim(my_tail10/2).
prim(my_pred11/2).
prim(my_sumlist12/2).
prim(my_succ13/2).
prim(my_succ14/2).
prim(my_succ15/2).
prim(my_reverse16/2).
prim(my_max_list17/2).
prim(my_succ18/2).
prim(my_tail19/2).
prim(my_pred20/2).
prim(my_sumlist21/2).
prim(my_max_list22/2).
prim(my_pred23/2).
prim(my_last24/2).
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
p([['x','u','j'],['s','b','c'],['u','b','s'],['p','p','u']],[['x','u'],['s','b'],['u','b'],['p','p']]).
p([['y','n','r'],['q','w','u','i'],['a','a','t']],[['y','n'],['q','w','u'],['a','a']]).
