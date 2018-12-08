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
my_succ0(A,B):-succ(A,B).
my_tail1([_|TL],TL).
my_pred2(A,B):-succ(B,A).
my_sumlist3(A,B):-sumlist(A,B).
my_max_list4(A,B):-max_list(A,B).
my_head5([H|_],H).
my_succ6(A,B):-succ(A,B).
my_min_list7(A,B):-min_list(A,B).
my_succ8(A,B):-succ(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_min_list10(A,B):-min_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_min_list14(A,B):-min_list(A,B).
my_head15([H|_],H).
my_sumlist16(A,B):-sumlist(A,B).
my_succ17(A,B):-succ(A,B).
my_reverse18(A,B):-reverse(A,B).
my_succ19(A,B):-succ(A,B).
prim(my_succ0/2).
prim(my_tail1/2).
prim(my_pred2/2).
prim(my_sumlist3/2).
prim(my_max_list4/2).
prim(my_head5/2).
prim(my_succ6/2).
prim(my_min_list7/2).
prim(my_succ8/2).
prim(my_sumlist9/2).
prim(my_min_list10/2).
prim(my_min_list11/2).
prim(my_reverse12/2).
prim(my_len13/2).
prim(my_min_list14/2).
prim(my_head15/2).
prim(my_sumlist16/2).
prim(my_succ17/2).
prim(my_reverse18/2).
prim(my_succ19/2).
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
p([['q','q','m'],['d','n','j','y'],['f','p','h'],['a','r','r']],[['q','q'],['d','n','j'],['f','p'],['a','r']]).
p([['l','w','s','h'],['o','r','g'],['n','d','x'],['e','v','q']],[['l','w','s'],['o','r'],['n','d'],['e','v']]).
