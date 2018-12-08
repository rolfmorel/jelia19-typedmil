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
my_pred1(A,B):-succ(B,A).
my_min_list2(A,B):-min_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_max_list6(A,B):-max_list(A,B).
my_succ7(A,B):-succ(A,B).
my_head8([H|_],H).
my_len9(A,B):-length(A,B).
my_len10(A,B):-length(A,B).
my_tail11([_|TL],TL).
my_min_list12(A,B):-min_list(A,B).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_head15([H|_],H).
my_succ16(A,B):-succ(A,B).
my_head17([H|_],H).
my_sumlist18(A,B):-sumlist(A,B).
my_min_list19(A,B):-min_list(A,B).
prim(my_len0/2).
prim(my_pred1/2).
prim(my_min_list2/2).
prim(my_max_list3/2).
prim(my_min_list4/2).
prim(my_sumlist5/2).
prim(my_max_list6/2).
prim(my_succ7/2).
prim(my_head8/2).
prim(my_len9/2).
prim(my_len10/2).
prim(my_tail11/2).
prim(my_min_list12/2).
prim(my_pred13/2).
prim(my_pred14/2).
prim(my_head15/2).
prim(my_succ16/2).
prim(my_head17/2).
prim(my_sumlist18/2).
prim(my_min_list19/2).
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
p([['v','l','y','p'],['o','t','p'],['l','a','l','x'],['h','i','g']],[['v','l','y'],['o','t'],['l','a','l'],['h','i']]).
p([['e','l','t'],['n','g','d','a']],[['e','l'],['n','g','d']]).
