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
my_head0([H|_],H).
my_pred1(A,B):-succ(B,A).
my_pred2(A,B):-succ(B,A).
my_len3(A,B):-length(A,B).
my_head4([H|_],H).
my_head5([H|_],H).
my_len6(A,B):-length(A,B).
my_head7([H|_],H).
my_reverse8(A,B):-reverse(A,B).
my_tail9([_|TL],TL).
my_len10(A,B):-length(A,B).
my_reverse11(A,B):-reverse(A,B).
my_len12(A,B):-length(A,B).
prim(my_head0/2).
prim(my_pred1/2).
prim(my_pred2/2).
prim(my_len3/2).
prim(my_head4/2).
prim(my_head5/2).
prim(my_len6/2).
prim(my_head7/2).
prim(my_reverse8/2).
prim(my_tail9/2).
prim(my_len10/2).
prim(my_reverse11/2).
prim(my_len12/2).
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
p([['q','n','m'],['o','j','j','h'],['j','k','p'],['f','d','c']],[['q','n'],['o','j','j'],['j','k'],['f','d']]).
p([['x','n','b'],['x','c','r'],['p','w','a']],[['x','n'],['x','c'],['p','w']]).
