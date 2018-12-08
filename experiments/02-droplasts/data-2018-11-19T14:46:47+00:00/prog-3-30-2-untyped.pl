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
my_pred2(A,B):-succ(B,A).
my_succ3(A,B):-succ(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_succ8(A,B):-succ(A,B).
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A).
my_last11(A,B):-last(A,B).
my_head12([H|_],H).
my_reverse13(A,B):-reverse(A,B).
my_min_list14(A,B):-min_list(A,B).
my_head15([H|_],H).
my_reverse16(A,B):-reverse(A,B).
my_head17([H|_],H).
my_len18(A,B):-length(A,B).
my_min_list19(A,B):-min_list(A,B).
my_len20(A,B):-length(A,B).
my_min_list21(A,B):-min_list(A,B).
my_len22(A,B):-length(A,B).
my_reverse23(A,B):-reverse(A,B).
my_len24(A,B):-length(A,B).
my_reverse25(A,B):-reverse(A,B).
my_last26(A,B):-last(A,B).
my_len27(A,B):-length(A,B).
my_sumlist28(A,B):-sumlist(A,B).
my_succ29(A,B):-succ(A,B).
prim(my_min_list0/2).
prim(my_min_list1/2).
prim(my_pred2/2).
prim(my_succ3/2).
prim(my_max_list4/2).
prim(my_len5/2).
prim(my_head6/2).
prim(my_tail7/2).
prim(my_succ8/2).
prim(my_last9/2).
prim(my_pred10/2).
prim(my_last11/2).
prim(my_head12/2).
prim(my_reverse13/2).
prim(my_min_list14/2).
prim(my_head15/2).
prim(my_reverse16/2).
prim(my_head17/2).
prim(my_len18/2).
prim(my_min_list19/2).
prim(my_len20/2).
prim(my_min_list21/2).
prim(my_len22/2).
prim(my_reverse23/2).
prim(my_len24/2).
prim(my_reverse25/2).
prim(my_last26/2).
prim(my_len27/2).
prim(my_sumlist28/2).
prim(my_succ29/2).
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
p([['v','e','w'],['p','n','i','x'],['o','l','e']],[['v','e'],['p','n','i'],['o','l']]).
p([['a','o','g','j'],['c','m','l'],['l','a','n']],[['a','o','g'],['c','m'],['l','a']]).
