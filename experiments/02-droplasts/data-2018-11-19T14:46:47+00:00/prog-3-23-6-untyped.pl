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
my_reverse1(A,B):-reverse(A,B).
my_min_list2(A,B):-min_list(A,B).
my_reverse3(A,B):-reverse(A,B).
my_pred4(A,B):-succ(B,A).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_last7(A,B):-last(A,B).
my_reverse8(A,B):-reverse(A,B).
my_head9([H|_],H).
my_reverse10(A,B):-reverse(A,B).
my_succ11(A,B):-succ(A,B).
my_len12(A,B):-length(A,B).
my_len13(A,B):-length(A,B).
my_succ14(A,B):-succ(A,B).
my_last15(A,B):-last(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_reverse17(A,B):-reverse(A,B).
my_len18(A,B):-length(A,B).
my_len19(A,B):-length(A,B).
my_succ20(A,B):-succ(A,B).
my_min_list21(A,B):-min_list(A,B).
my_head22([H|_],H).
prim(my_min_list0/2).
prim(my_reverse1/2).
prim(my_min_list2/2).
prim(my_reverse3/2).
prim(my_pred4/2).
prim(my_len5/2).
prim(my_min_list6/2).
prim(my_last7/2).
prim(my_reverse8/2).
prim(my_head9/2).
prim(my_reverse10/2).
prim(my_succ11/2).
prim(my_len12/2).
prim(my_len13/2).
prim(my_succ14/2).
prim(my_last15/2).
prim(my_sumlist16/2).
prim(my_reverse17/2).
prim(my_len18/2).
prim(my_len19/2).
prim(my_succ20/2).
prim(my_min_list21/2).
prim(my_head22/2).
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
p([['o','l','w','a'],['h','y','r','l']],[['o','l','w'],['h','y','r']]).
p([['u','h','k','x'],['y','h','o','x'],['j','q','e']],[['u','h','k'],['y','h','o'],['j','q']]).
