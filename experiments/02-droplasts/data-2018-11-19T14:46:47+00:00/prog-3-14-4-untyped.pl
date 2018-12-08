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
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_head3([H|_],H).
my_head4([H|_],H).
my_tail5([_|TL],TL).
my_sumlist6(A,B):-sumlist(A,B).
my_min_list7(A,B):-min_list(A,B).
my_last8(A,B):-last(A,B).
my_succ9(A,B):-succ(A,B).
my_len10(A,B):-length(A,B).
my_pred11(A,B):-succ(B,A).
my_reverse12(A,B):-reverse(A,B).
my_head13([H|_],H).
prim(my_max_list0/2).
prim(my_tail1/2).
prim(my_min_list2/2).
prim(my_head3/2).
prim(my_head4/2).
prim(my_tail5/2).
prim(my_sumlist6/2).
prim(my_min_list7/2).
prim(my_last8/2).
prim(my_succ9/2).
prim(my_len10/2).
prim(my_pred11/2).
prim(my_reverse12/2).
prim(my_head13/2).
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
p([['i','p','i','a'],['h','c','d','o'],['j','b','r','c']],[['i','p','i'],['h','c','d'],['j','b','r']]).
p([['a','i','o','q'],['w','a','f','t'],['x','d','f']],[['a','i','o'],['w','a','f'],['x','d']]).
