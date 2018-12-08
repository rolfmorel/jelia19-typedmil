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
my_max_list2(A,B):-max_list(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_pred4(A,B):-succ(B,A).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_max_list7(A,B):-max_list(A,B).
my_pred8(A,B):-succ(B,A).
my_tail9([_|TL],TL).
my_max_list10(A,B):-max_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_tail12([_|TL],TL).
my_head13([H|_],H).
my_tail14([_|TL],TL).
my_tail15([_|TL],TL).
my_tail16([_|TL],TL).
my_reverse17(A,B):-reverse(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_tail21([_|TL],TL).
my_head22([H|_],H).
my_last23(A,B):-last(A,B).
my_tail24([_|TL],TL).
my_tail25([_|TL],TL).
my_len26(A,B):-length(A,B).
my_pred27(A,B):-succ(B,A).
my_last28(A,B):-last(A,B).
prim(my_min_list0/2).
prim(my_reverse1/2).
prim(my_max_list2/2).
prim(my_sumlist3/2).
prim(my_pred4/2).
prim(my_min_list5/2).
prim(my_tail6/2).
prim(my_max_list7/2).
prim(my_pred8/2).
prim(my_tail9/2).
prim(my_max_list10/2).
prim(my_min_list11/2).
prim(my_tail12/2).
prim(my_head13/2).
prim(my_tail14/2).
prim(my_tail15/2).
prim(my_tail16/2).
prim(my_reverse17/2).
prim(my_sumlist18/2).
prim(my_sumlist19/2).
prim(my_sumlist20/2).
prim(my_tail21/2).
prim(my_head22/2).
prim(my_last23/2).
prim(my_tail24/2).
prim(my_tail25/2).
prim(my_len26/2).
prim(my_pred27/2).
prim(my_last28/2).
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
p([['n','m','d','j'],['o','u','a'],['c','e','c'],['m','c','p','f']],[['n','m','d'],['o','u'],['c','e'],['m','c','p']]).
p([['e','f','g'],['a','x','c','p'],['e','y','f'],['j','t','j','e']],[['e','f'],['a','x','c'],['e','y'],['j','t','j']]).
