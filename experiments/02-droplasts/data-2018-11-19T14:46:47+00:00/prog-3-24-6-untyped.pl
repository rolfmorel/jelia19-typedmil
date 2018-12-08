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
my_succ1(A,B):-succ(A,B).
my_max_list2(A,B):-max_list(A,B).
my_min_list3(A,B):-min_list(A,B).
my_pred4(A,B):-succ(B,A).
my_pred5(A,B):-succ(B,A).
my_last6(A,B):-last(A,B).
my_len7(A,B):-length(A,B).
my_head8([H|_],H).
my_len9(A,B):-length(A,B).
my_len10(A,B):-length(A,B).
my_len11(A,B):-length(A,B).
my_head12([H|_],H).
my_len13(A,B):-length(A,B).
my_tail14([_|TL],TL).
my_len15(A,B):-length(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_len17(A,B):-length(A,B).
my_reverse18(A,B):-reverse(A,B).
my_tail19([_|TL],TL).
my_last20(A,B):-last(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_succ22(A,B):-succ(A,B).
my_reverse23(A,B):-reverse(A,B).
prim(my_tail0/2).
prim(my_succ1/2).
prim(my_max_list2/2).
prim(my_min_list3/2).
prim(my_pred4/2).
prim(my_pred5/2).
prim(my_last6/2).
prim(my_len7/2).
prim(my_head8/2).
prim(my_len9/2).
prim(my_len10/2).
prim(my_len11/2).
prim(my_head12/2).
prim(my_len13/2).
prim(my_tail14/2).
prim(my_len15/2).
prim(my_sumlist16/2).
prim(my_len17/2).
prim(my_reverse18/2).
prim(my_tail19/2).
prim(my_last20/2).
prim(my_sumlist21/2).
prim(my_succ22/2).
prim(my_reverse23/2).
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
p([['q','t','t','r'],['n','e','d','d'],['c','k','a'],['u','f','e','h']],[['q','t','t'],['n','e','d'],['c','k'],['u','f','e']]).
p([['g','y','d'],['o','l','h','i']],[['g','y'],['o','l','h']]).
