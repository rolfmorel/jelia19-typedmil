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
my_tail1([_|TL],TL).
my_last2(A,B):-last(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_head4([H|_],H).
my_sumlist5(A,B):-sumlist(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_min_list8(A,B):-min_list(A,B).
my_reverse9(A,B):-reverse(A,B).
my_pred10(A,B):-succ(B,A).
my_min_list11(A,B):-min_list(A,B).
my_max_list12(A,B):-max_list(A,B).
my_len13(A,B):-length(A,B).
my_min_list14(A,B):-min_list(A,B).
my_min_list15(A,B):-min_list(A,B).
my_min_list16(A,B):-min_list(A,B).
my_last17(A,B):-last(A,B).
my_head18([H|_],H).
my_sumlist19(A,B):-sumlist(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_pred21(A,B):-succ(B,A).
my_reverse22(A,B):-reverse(A,B).
my_pred23(A,B):-succ(B,A).
prim(my_tail0/2).
prim(my_tail1/2).
prim(my_last2/2).
prim(my_sumlist3/2).
prim(my_head4/2).
prim(my_sumlist5/2).
prim(my_head6/2).
prim(my_tail7/2).
prim(my_min_list8/2).
prim(my_reverse9/2).
prim(my_pred10/2).
prim(my_min_list11/2).
prim(my_max_list12/2).
prim(my_len13/2).
prim(my_min_list14/2).
prim(my_min_list15/2).
prim(my_min_list16/2).
prim(my_last17/2).
prim(my_head18/2).
prim(my_sumlist19/2).
prim(my_sumlist20/2).
prim(my_pred21/2).
prim(my_reverse22/2).
prim(my_pred23/2).
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
p([['t','s','p','y'],['m','x','g']],[['t','s','p'],['m','x']]).
p([['e','u','g','v'],['d','x','m','g'],['l','b','s']],[['e','u','g'],['d','x','m'],['l','b']]).
