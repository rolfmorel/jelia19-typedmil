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
my_head0([H|_],H).
my_max_list1(A,B):-max_list(A,B).
my_succ2(A,B):-succ(A,B).
my_max_list3(A,B):-max_list(A,B).
my_tail4([_|TL],TL).
my_pred5(A,B):-succ(B,A).
my_sumlist6(A,B):-sumlist(A,B).
my_succ7(A,B):-succ(A,B).
my_reverse8(A,B):-reverse(A,B).
my_pred9(A,B):-succ(B,A).
my_len10(A,B):-length(A,B).
my_len11(A,B):-length(A,B).
my_pred12(A,B):-succ(B,A).
my_tail13([_|TL],TL).
my_min_list14(A,B):-min_list(A,B).
my_last15(A,B):-last(A,B).
my_min_list16(A,B):-min_list(A,B).
my_len17(A,B):-length(A,B).
my_reverse18(A,B):-reverse(A,B).
my_reverse19(A,B):-reverse(A,B).
my_pred20(A,B):-succ(B,A).
my_tail21([_|TL],TL).
my_len22(A,B):-length(A,B).
my_last23(A,B):-last(A,B).
my_head24([H|_],H).
my_pred25(A,B):-succ(B,A).
prim(my_head0/2).
prim(my_max_list1/2).
prim(my_succ2/2).
prim(my_max_list3/2).
prim(my_tail4/2).
prim(my_pred5/2).
prim(my_sumlist6/2).
prim(my_succ7/2).
prim(my_reverse8/2).
prim(my_pred9/2).
prim(my_len10/2).
prim(my_len11/2).
prim(my_pred12/2).
prim(my_tail13/2).
prim(my_min_list14/2).
prim(my_last15/2).
prim(my_min_list16/2).
prim(my_len17/2).
prim(my_reverse18/2).
prim(my_reverse19/2).
prim(my_pred20/2).
prim(my_tail21/2).
prim(my_len22/2).
prim(my_last23/2).
prim(my_head24/2).
prim(my_pred25/2).
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
p([['f','a','k'],['a','a','t','s'],['v','i','r','d'],['e','b','w']],[['f','a'],['a','a','t'],['v','i','r'],['e','b']]).
p([['u','b','g'],['l','q','p'],['v','i','k'],['l','v','o','f']],[['u','b'],['l','q'],['v','i'],['l','v','o']]).
