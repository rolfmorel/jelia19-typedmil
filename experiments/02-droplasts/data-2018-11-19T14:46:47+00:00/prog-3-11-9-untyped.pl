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
my_last0(A,B):-last(A,B).
my_reverse1(A,B):-reverse(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_last4(A,B):-last(A,B).
my_len5(A,B):-length(A,B).
my_last6(A,B):-last(A,B).
my_pred7(A,B):-succ(B,A).
my_tail8([_|TL],TL).
my_len9(A,B):-length(A,B).
my_last10(A,B):-last(A,B).
prim(my_last0/2).
prim(my_reverse1/2).
prim(my_sumlist2/2).
prim(my_reverse3/2).
prim(my_last4/2).
prim(my_len5/2).
prim(my_last6/2).
prim(my_pred7/2).
prim(my_tail8/2).
prim(my_len9/2).
prim(my_last10/2).
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
p([['e','g','o'],['n','k','o','e'],['q','r','j'],['r','o','k','v']],[['e','g'],['n','k','o'],['q','r'],['r','o','k']]).
p([['k','d','m','o'],['r','r','t','p']],[['k','d','m'],['r','r','t']]).
