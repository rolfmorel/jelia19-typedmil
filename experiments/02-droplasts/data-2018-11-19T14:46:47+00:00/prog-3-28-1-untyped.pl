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
my_tail1([_|TL],TL).
my_last2(A,B):-last(A,B).
my_min_list3(A,B):-min_list(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_succ5(A,B):-succ(A,B).
my_min_list6(A,B):-min_list(A,B).
my_pred7(A,B):-succ(B,A).
my_tail8([_|TL],TL).
my_tail9([_|TL],TL).
my_pred10(A,B):-succ(B,A).
my_tail11([_|TL],TL).
my_len12(A,B):-length(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_len14(A,B):-length(A,B).
my_pred15(A,B):-succ(B,A).
my_pred16(A,B):-succ(B,A).
my_reverse17(A,B):-reverse(A,B).
my_pred18(A,B):-succ(B,A).
my_max_list19(A,B):-max_list(A,B).
my_head20([H|_],H).
my_succ21(A,B):-succ(A,B).
my_len22(A,B):-length(A,B).
my_tail23([_|TL],TL).
my_pred24(A,B):-succ(B,A).
my_last25(A,B):-last(A,B).
my_sumlist26(A,B):-sumlist(A,B).
my_reverse27(A,B):-reverse(A,B).
prim(my_pred0/2).
prim(my_tail1/2).
prim(my_last2/2).
prim(my_min_list3/2).
prim(my_sumlist4/2).
prim(my_succ5/2).
prim(my_min_list6/2).
prim(my_pred7/2).
prim(my_tail8/2).
prim(my_tail9/2).
prim(my_pred10/2).
prim(my_tail11/2).
prim(my_len12/2).
prim(my_sumlist13/2).
prim(my_len14/2).
prim(my_pred15/2).
prim(my_pred16/2).
prim(my_reverse17/2).
prim(my_pred18/2).
prim(my_max_list19/2).
prim(my_head20/2).
prim(my_succ21/2).
prim(my_len22/2).
prim(my_tail23/2).
prim(my_pred24/2).
prim(my_last25/2).
prim(my_sumlist26/2).
prim(my_reverse27/2).
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
p([['l','x','w','x'],['j','n','q','h'],['e','w','f','g'],['s','n','c','o']],[['l','x','w'],['j','n','q'],['e','w','f'],['s','n','c']]).
p([['a','p','d'],['o','q','e'],['o','y','y','d'],['b','e','g','t']],[['a','p'],['o','q'],['o','y','y'],['b','e','g']]).
