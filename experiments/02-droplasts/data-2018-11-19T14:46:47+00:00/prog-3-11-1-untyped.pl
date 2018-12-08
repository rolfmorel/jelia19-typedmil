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
my_succ1(A,B):-succ(A,B).
my_pred2(A,B):-succ(B,A).
my_reverse3(A,B):-reverse(A,B).
my_tail4([_|TL],TL).
my_pred5(A,B):-succ(B,A).
my_pred6(A,B):-succ(B,A).
my_head7([H|_],H).
my_max_list8(A,B):-max_list(A,B).
my_len9(A,B):-length(A,B).
my_last10(A,B):-last(A,B).
prim(my_succ0/2).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_reverse3/2).
prim(my_tail4/2).
prim(my_pred5/2).
prim(my_pred6/2).
prim(my_head7/2).
prim(my_max_list8/2).
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
p([['r','d','r'],['o','d','j','p']],[['r','d'],['o','d','j']]).
p([['b','v','t'],['e','r','k','h']],[['b','v'],['e','r','k']]).
