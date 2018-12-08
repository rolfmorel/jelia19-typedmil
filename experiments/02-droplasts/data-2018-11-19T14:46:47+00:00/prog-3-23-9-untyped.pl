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
my_pred1(A,B):-succ(B,A).
my_head2([H|_],H).
my_max_list3(A,B):-max_list(A,B).
my_pred4(A,B):-succ(B,A).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A).
my_reverse7(A,B):-reverse(A,B).
my_pred8(A,B):-succ(B,A).
my_sumlist9(A,B):-sumlist(A,B).
my_last10(A,B):-last(A,B).
my_tail11([_|TL],TL).
my_head12([H|_],H).
my_max_list13(A,B):-max_list(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_min_list15(A,B):-min_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_len18(A,B):-length(A,B).
my_pred19(A,B):-succ(B,A).
my_reverse20(A,B):-reverse(A,B).
my_last21(A,B):-last(A,B).
my_reverse22(A,B):-reverse(A,B).
prim(my_min_list0/2).
prim(my_pred1/2).
prim(my_head2/2).
prim(my_max_list3/2).
prim(my_pred4/2).
prim(my_max_list5/2).
prim(my_pred6/2).
prim(my_reverse7/2).
prim(my_pred8/2).
prim(my_sumlist9/2).
prim(my_last10/2).
prim(my_tail11/2).
prim(my_head12/2).
prim(my_max_list13/2).
prim(my_sumlist14/2).
prim(my_min_list15/2).
prim(my_reverse16/2).
prim(my_sumlist17/2).
prim(my_len18/2).
prim(my_pred19/2).
prim(my_reverse20/2).
prim(my_last21/2).
prim(my_reverse22/2).
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
p([['j','e','g'],['g','d','r','w'],['i','j','a','d']],[['j','e'],['g','d','r'],['i','j','a']]).
p([['s','h','b'],['j','q','u'],['e','o','c'],['v','p','r']],[['s','h'],['j','q'],['e','o'],['v','p']]).
