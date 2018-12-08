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
my_min_list1(A,B):-min_list(A,B).
my_reverse2(A,B):-reverse(A,B).
my_len3(A,B):-length(A,B).
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B).
my_head7([H|_],H).
my_last8(A,B):-last(A,B).
my_succ9(A,B):-succ(A,B).
my_min_list10(A,B):-min_list(A,B).
my_head11([H|_],H).
my_sumlist12(A,B):-sumlist(A,B).
my_head13([H|_],H).
my_head14([H|_],H).
my_sumlist15(A,B):-sumlist(A,B).
my_tail16([_|TL],TL).
prim(my_max_list0/2).
prim(my_min_list1/2).
prim(my_reverse2/2).
prim(my_len3/2).
prim(my_min_list4/2).
prim(my_sumlist5/2).
prim(my_succ6/2).
prim(my_head7/2).
prim(my_last8/2).
prim(my_succ9/2).
prim(my_min_list10/2).
prim(my_head11/2).
prim(my_sumlist12/2).
prim(my_head13/2).
prim(my_head14/2).
prim(my_sumlist15/2).
prim(my_tail16/2).
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
p([['o','b','r','w'],['x','e','u','x']],[['o','b','r'],['x','e','u']]).
p([['t','v','y','x'],['e','e','f'],['h','v','f'],['b','r','m','p']],[['t','v','y'],['e','e'],['h','v'],['b','r','m']]).
