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
my_max_list0(A,B):-max_list(A,B).
my_head1([H|_],H).
my_pred2(A,B):-succ(B,A).
my_max_list3(A,B):-max_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A).
my_last7(A,B):-last(A,B).
my_last8(A,B):-last(A,B).
my_len9(A,B):-length(A,B).
my_tail10([_|TL],TL).
my_max_list11(A,B):-max_list(A,B).
my_pred12(A,B):-succ(B,A).
my_min_list13(A,B):-min_list(A,B).
my_succ14(A,B):-succ(A,B).
my_succ15(A,B):-succ(A,B).
my_succ16(A,B):-succ(A,B).
my_reverse17(A,B):-reverse(A,B).
my_min_list18(A,B):-min_list(A,B).
my_pred19(A,B):-succ(B,A).
my_pred20(A,B):-succ(B,A).
my_succ21(A,B):-succ(A,B).
my_last22(A,B):-last(A,B).
my_sumlist23(A,B):-sumlist(A,B).
prim(my_max_list0/2).
prim(my_head1/2).
prim(my_pred2/2).
prim(my_max_list3/2).
prim(my_max_list4/2).
prim(my_max_list5/2).
prim(my_pred6/2).
prim(my_last7/2).
prim(my_last8/2).
prim(my_len9/2).
prim(my_tail10/2).
prim(my_max_list11/2).
prim(my_pred12/2).
prim(my_min_list13/2).
prim(my_succ14/2).
prim(my_succ15/2).
prim(my_succ16/2).
prim(my_reverse17/2).
prim(my_min_list18/2).
prim(my_pred19/2).
prim(my_pred20/2).
prim(my_succ21/2).
prim(my_last22/2).
prim(my_sumlist23/2).
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
p([['d','a','s','g'],['j','n','u']],[['d','a','s'],['j','n']]).
p([['x','x','b'],['w','t','p','v'],['l','u','b','m']],[['x','x'],['w','t','p'],['l','u','b']]).
