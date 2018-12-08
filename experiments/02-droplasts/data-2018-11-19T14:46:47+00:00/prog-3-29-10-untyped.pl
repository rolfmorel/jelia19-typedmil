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
my_len1(A,B):-length(A,B).
my_succ2(A,B):-succ(A,B).
my_max_list3(A,B):-max_list(A,B).
my_len4(A,B):-length(A,B).
my_tail5([_|TL],TL).
my_head6([H|_],H).
my_last7(A,B):-last(A,B).
my_min_list8(A,B):-min_list(A,B).
my_reverse9(A,B):-reverse(A,B).
my_succ10(A,B):-succ(A,B).
my_max_list11(A,B):-max_list(A,B).
my_head12([H|_],H).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_reverse15(A,B):-reverse(A,B).
my_head16([H|_],H).
my_sumlist17(A,B):-sumlist(A,B).
my_len18(A,B):-length(A,B).
my_succ19(A,B):-succ(A,B).
my_pred20(A,B):-succ(B,A).
my_reverse21(A,B):-reverse(A,B).
my_min_list22(A,B):-min_list(A,B).
my_succ23(A,B):-succ(A,B).
my_sumlist24(A,B):-sumlist(A,B).
my_last25(A,B):-last(A,B).
my_tail26([_|TL],TL).
my_last27(A,B):-last(A,B).
my_pred28(A,B):-succ(B,A).
prim(my_pred0/2).
prim(my_len1/2).
prim(my_succ2/2).
prim(my_max_list3/2).
prim(my_len4/2).
prim(my_tail5/2).
prim(my_head6/2).
prim(my_last7/2).
prim(my_min_list8/2).
prim(my_reverse9/2).
prim(my_succ10/2).
prim(my_max_list11/2).
prim(my_head12/2).
prim(my_pred13/2).
prim(my_pred14/2).
prim(my_reverse15/2).
prim(my_head16/2).
prim(my_sumlist17/2).
prim(my_len18/2).
prim(my_succ19/2).
prim(my_pred20/2).
prim(my_reverse21/2).
prim(my_min_list22/2).
prim(my_succ23/2).
prim(my_sumlist24/2).
prim(my_last25/2).
prim(my_tail26/2).
prim(my_last27/2).
prim(my_pred28/2).
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
p([['e','l','v','v'],['q','y','n','m']],[['e','l','v'],['q','y','n']]).
p([['j','g','t','k'],['d','x','d','o']],[['j','g','t'],['d','x','d']]).
