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
my_head1([H|_],H).
my_max_list2(A,B):-max_list(A,B).
my_last3(A,B):-last(A,B).
my_tail4([_|TL],TL).
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_succ7(A,B):-succ(A,B).
my_head8([H|_],H).
my_max_list9(A,B):-max_list(A,B).
my_tail10([_|TL],TL).
my_pred11(A,B):-succ(B,A).
my_sumlist12(A,B):-sumlist(A,B).
my_pred13(A,B):-succ(B,A).
my_sumlist14(A,B):-sumlist(A,B).
my_tail15([_|TL],TL).
my_reverse16(A,B):-reverse(A,B).
my_head17([H|_],H).
my_min_list18(A,B):-min_list(A,B).
my_last19(A,B):-last(A,B).
my_succ20(A,B):-succ(A,B).
my_last21(A,B):-last(A,B).
my_min_list22(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_max_list2/2).
prim(my_last3/2).
prim(my_tail4/2).
prim(my_last5/2).
prim(my_min_list6/2).
prim(my_succ7/2).
prim(my_head8/2).
prim(my_max_list9/2).
prim(my_tail10/2).
prim(my_pred11/2).
prim(my_sumlist12/2).
prim(my_pred13/2).
prim(my_sumlist14/2).
prim(my_tail15/2).
prim(my_reverse16/2).
prim(my_head17/2).
prim(my_min_list18/2).
prim(my_last19/2).
prim(my_succ20/2).
prim(my_last21/2).
prim(my_min_list22/2).
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
p([['i','i','k'],['d','o','c'],['m','w','w','t']],[['i','i'],['d','o'],['m','w','w']]).
p([['y','j','t'],['s','s','s'],['k','y','o','f'],['k','d','d','a']],[['y','j'],['s','s'],['k','y','o'],['k','d','d']]).
