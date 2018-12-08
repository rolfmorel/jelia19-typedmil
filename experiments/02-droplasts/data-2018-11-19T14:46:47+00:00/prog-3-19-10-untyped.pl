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
my_max_list1(A,B):-max_list(A,B).
my_tail2([_|TL],TL).
my_sumlist3(A,B):-sumlist(A,B).
my_len4(A,B):-length(A,B).
my_succ5(A,B):-succ(A,B).
my_last6(A,B):-last(A,B).
my_succ7(A,B):-succ(A,B).
my_succ8(A,B):-succ(A,B).
my_max_list9(A,B):-max_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_head11([H|_],H).
my_succ12(A,B):-succ(A,B).
my_reverse13(A,B):-reverse(A,B).
my_reverse14(A,B):-reverse(A,B).
my_reverse15(A,B):-reverse(A,B).
my_min_list16(A,B):-min_list(A,B).
my_succ17(A,B):-succ(A,B).
my_reverse18(A,B):-reverse(A,B).
prim(my_sumlist0/2).
prim(my_max_list1/2).
prim(my_tail2/2).
prim(my_sumlist3/2).
prim(my_len4/2).
prim(my_succ5/2).
prim(my_last6/2).
prim(my_succ7/2).
prim(my_succ8/2).
prim(my_max_list9/2).
prim(my_reverse10/2).
prim(my_head11/2).
prim(my_succ12/2).
prim(my_reverse13/2).
prim(my_reverse14/2).
prim(my_reverse15/2).
prim(my_min_list16/2).
prim(my_succ17/2).
prim(my_reverse18/2).
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
p([['v','d','w'],['u','a','b'],['i','l','d'],['f','i','o','h']],[['v','d'],['u','a'],['i','l'],['f','i','o']]).
p([['j','f','m','m'],['p','u','e','u']],[['j','f','m'],['p','u','e']]).
