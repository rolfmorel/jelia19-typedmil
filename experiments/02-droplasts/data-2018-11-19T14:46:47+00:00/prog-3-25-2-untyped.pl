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
my_reverse1(A,B):-reverse(A,B).
my_min_list2(A,B):-min_list(A,B).
my_pred3(A,B):-succ(B,A).
my_head4([H|_],H).
my_succ5(A,B):-succ(A,B).
my_min_list6(A,B):-min_list(A,B).
my_len7(A,B):-length(A,B).
my_min_list8(A,B):-min_list(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_succ10(A,B):-succ(A,B).
my_tail11([_|TL],TL).
my_last12(A,B):-last(A,B).
my_pred13(A,B):-succ(B,A).
my_min_list14(A,B):-min_list(A,B).
my_succ15(A,B):-succ(A,B).
my_head16([H|_],H).
my_max_list17(A,B):-max_list(A,B).
my_reverse18(A,B):-reverse(A,B).
my_min_list19(A,B):-min_list(A,B).
my_reverse20(A,B):-reverse(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_max_list22(A,B):-max_list(A,B).
my_max_list23(A,B):-max_list(A,B).
my_succ24(A,B):-succ(A,B).
prim(my_max_list0/2).
prim(my_reverse1/2).
prim(my_min_list2/2).
prim(my_pred3/2).
prim(my_head4/2).
prim(my_succ5/2).
prim(my_min_list6/2).
prim(my_len7/2).
prim(my_min_list8/2).
prim(my_sumlist9/2).
prim(my_succ10/2).
prim(my_tail11/2).
prim(my_last12/2).
prim(my_pred13/2).
prim(my_min_list14/2).
prim(my_succ15/2).
prim(my_head16/2).
prim(my_max_list17/2).
prim(my_reverse18/2).
prim(my_min_list19/2).
prim(my_reverse20/2).
prim(my_sumlist21/2).
prim(my_max_list22/2).
prim(my_max_list23/2).
prim(my_succ24/2).
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
p([['v','d','q','x'],['p','a','l'],['u','v','x'],['c','v','f']],[['v','d','q'],['p','a'],['u','v'],['c','v']]).
p([['m','n','x'],['w','k','y','v']],[['m','n'],['w','k','y']]).
