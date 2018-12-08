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
my_pred2(A,B):-succ(B,A).
my_min_list3(A,B):-min_list(A,B).
my_pred4(A,B):-succ(B,A).
my_sumlist5(A,B):-sumlist(A,B).
my_reverse6(A,B):-reverse(A,B).
my_min_list7(A,B):-min_list(A,B).
my_max_list8(A,B):-max_list(A,B).
my_max_list9(A,B):-max_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_min_list11(A,B):-min_list(A,B).
my_last12(A,B):-last(A,B).
prim(my_max_list0/2).
prim(my_reverse1/2).
prim(my_pred2/2).
prim(my_min_list3/2).
prim(my_pred4/2).
prim(my_sumlist5/2).
prim(my_reverse6/2).
prim(my_min_list7/2).
prim(my_max_list8/2).
prim(my_max_list9/2).
prim(my_reverse10/2).
prim(my_min_list11/2).
prim(my_last12/2).
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
p([['g','y','t','u'],['c','q','j','w'],['w','u','l','d']],[['g','y','t'],['c','q','j'],['w','u','l']]).
p([['b','k','c'],['b','c','m'],['n','d','w']],[['b','k'],['b','c'],['n','d']]).
