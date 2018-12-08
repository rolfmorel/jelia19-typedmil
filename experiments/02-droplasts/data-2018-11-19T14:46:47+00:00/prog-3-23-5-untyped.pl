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
my_pred0(A,B):-succ(B,A).
my_last1(A,B):-last(A,B).
my_head2([H|_],H).
my_sumlist3(A,B):-sumlist(A,B).
my_succ4(A,B):-succ(A,B).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_max_list7(A,B):-max_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_succ9(A,B):-succ(A,B).
my_pred10(A,B):-succ(B,A).
my_tail11([_|TL],TL).
my_succ12(A,B):-succ(A,B).
my_tail13([_|TL],TL).
my_reverse14(A,B):-reverse(A,B).
my_max_list15(A,B):-max_list(A,B).
my_reverse16(A,B):-reverse(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_tail18([_|TL],TL).
my_len19(A,B):-length(A,B).
my_last20(A,B):-last(A,B).
my_min_list21(A,B):-min_list(A,B).
my_max_list22(A,B):-max_list(A,B).
prim(my_pred0/2).
prim(my_last1/2).
prim(my_head2/2).
prim(my_sumlist3/2).
prim(my_succ4/2).
prim(my_min_list5/2).
prim(my_tail6/2).
prim(my_max_list7/2).
prim(my_reverse8/2).
prim(my_succ9/2).
prim(my_pred10/2).
prim(my_tail11/2).
prim(my_succ12/2).
prim(my_tail13/2).
prim(my_reverse14/2).
prim(my_max_list15/2).
prim(my_reverse16/2).
prim(my_sumlist17/2).
prim(my_tail18/2).
prim(my_len19/2).
prim(my_last20/2).
prim(my_min_list21/2).
prim(my_max_list22/2).
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
p([['c','j','l','e'],['m','t','f','b'],['y','u','v'],['x','a','o','y']],[['c','j','l'],['m','t','f'],['y','u'],['x','a','o']]).
p([['h','p','r','o'],['b','l','d','c'],['k','m','q'],['f','p','m','b']],[['h','p','r'],['b','l','d'],['k','m'],['f','p','m']]).
