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
my_max_list1(A,B):-max_list(A,B).
my_len2(A,B):-length(A,B).
my_max_list3(A,B):-max_list(A,B).
my_reverse4(A,B):-reverse(A,B).
my_len5(A,B):-length(A,B).
my_last6(A,B):-last(A,B).
my_min_list7(A,B):-min_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_max_list11(A,B):-max_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_reverse13(A,B):-reverse(A,B).
my_head14([H|_],H).
my_tail15([_|TL],TL).
my_min_list16(A,B):-min_list(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_len18(A,B):-length(A,B).
my_reverse19(A,B):-reverse(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_succ21(A,B):-succ(A,B).
my_reverse22(A,B):-reverse(A,B).
my_max_list23(A,B):-max_list(A,B).
my_tail24([_|TL],TL).
my_len25(A,B):-length(A,B).
my_head26([H|_],H).
my_last27(A,B):-last(A,B).
prim(my_pred0/2).
prim(my_max_list1/2).
prim(my_len2/2).
prim(my_max_list3/2).
prim(my_reverse4/2).
prim(my_len5/2).
prim(my_last6/2).
prim(my_min_list7/2).
prim(my_sumlist8/2).
prim(my_min_list9/2).
prim(my_reverse10/2).
prim(my_max_list11/2).
prim(my_reverse12/2).
prim(my_reverse13/2).
prim(my_head14/2).
prim(my_tail15/2).
prim(my_min_list16/2).
prim(my_sumlist17/2).
prim(my_len18/2).
prim(my_reverse19/2).
prim(my_sumlist20/2).
prim(my_succ21/2).
prim(my_reverse22/2).
prim(my_max_list23/2).
prim(my_tail24/2).
prim(my_len25/2).
prim(my_head26/2).
prim(my_last27/2).
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
p([['t','q','g','h'],['w','j','q'],['c','n','v','d'],['m','c','w','k']],[['t','q','g'],['w','j'],['c','n','v'],['m','c','w']]).
p([['f','w','i','m'],['s','o','b','g']],[['f','w','i'],['s','o','b']]).
