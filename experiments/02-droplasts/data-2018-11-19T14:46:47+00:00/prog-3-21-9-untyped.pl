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
my_len0(A,B):-length(A,B).
my_max_list1(A,B):-max_list(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_min_list3(A,B):-min_list(A,B).
my_last4(A,B):-last(A,B).
my_head5([H|_],H).
my_max_list6(A,B):-max_list(A,B).
my_last7(A,B):-last(A,B).
my_head8([H|_],H).
my_len9(A,B):-length(A,B).
my_len10(A,B):-length(A,B).
my_min_list11(A,B):-min_list(A,B).
my_max_list12(A,B):-max_list(A,B).
my_head13([H|_],H).
my_head14([H|_],H).
my_reverse15(A,B):-reverse(A,B).
my_head16([H|_],H).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_len19(A,B):-length(A,B).
my_pred20(A,B):-succ(B,A).
prim(my_len0/2).
prim(my_max_list1/2).
prim(my_sumlist2/2).
prim(my_min_list3/2).
prim(my_last4/2).
prim(my_head5/2).
prim(my_max_list6/2).
prim(my_last7/2).
prim(my_head8/2).
prim(my_len9/2).
prim(my_len10/2).
prim(my_min_list11/2).
prim(my_max_list12/2).
prim(my_head13/2).
prim(my_head14/2).
prim(my_reverse15/2).
prim(my_head16/2).
prim(my_head17/2).
prim(my_sumlist18/2).
prim(my_len19/2).
prim(my_pred20/2).
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
p([['q','g','w','y'],['m','x','s'],['u','k','x','g'],['l','t','r','q']],[['q','g','w'],['m','x'],['u','k','x'],['l','t','r']]).
p([['c','h','r'],['n','m','c']],[['c','h'],['n','m']]).
