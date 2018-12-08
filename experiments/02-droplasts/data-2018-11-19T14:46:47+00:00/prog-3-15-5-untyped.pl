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
my_len1(A,B):-length(A,B).
my_pred2(A,B):-succ(B,A).
my_head3([H|_],H).
my_max_list4(A,B):-max_list(A,B).
my_max_list5(A,B):-max_list(A,B).
my_tail6([_|TL],TL).
my_tail7([_|TL],TL).
my_succ8(A,B):-succ(A,B).
my_pred9(A,B):-succ(B,A).
my_len10(A,B):-length(A,B).
my_reverse11(A,B):-reverse(A,B).
my_pred12(A,B):-succ(B,A).
my_tail13([_|TL],TL).
my_min_list14(A,B):-min_list(A,B).
prim(my_sumlist0/2).
prim(my_len1/2).
prim(my_pred2/2).
prim(my_head3/2).
prim(my_max_list4/2).
prim(my_max_list5/2).
prim(my_tail6/2).
prim(my_tail7/2).
prim(my_succ8/2).
prim(my_pred9/2).
prim(my_len10/2).
prim(my_reverse11/2).
prim(my_pred12/2).
prim(my_tail13/2).
prim(my_min_list14/2).
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
p([['o','v','b'],['n','p','n','i'],['a','t','c'],['q','a','r','i']],[['o','v'],['n','p','n'],['a','t'],['q','a','r']]).
p([['l','e','n'],['b','j','v','c'],['p','h','k']],[['l','e'],['b','j','v'],['p','h']]).
