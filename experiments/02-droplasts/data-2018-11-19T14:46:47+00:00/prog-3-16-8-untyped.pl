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
my_reverse1(A,B):-reverse(A,B).
my_reverse2(A,B):-reverse(A,B).
my_reverse3(A,B):-reverse(A,B).
my_succ4(A,B):-succ(A,B).
my_len5(A,B):-length(A,B).
my_min_list6(A,B):-min_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_head8([H|_],H).
my_last9(A,B):-last(A,B).
my_min_list10(A,B):-min_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_last12(A,B):-last(A,B).
my_pred13(A,B):-succ(B,A).
my_reverse14(A,B):-reverse(A,B).
my_tail15([_|TL],TL).
prim(my_tail0/2).
prim(my_reverse1/2).
prim(my_reverse2/2).
prim(my_reverse3/2).
prim(my_succ4/2).
prim(my_len5/2).
prim(my_min_list6/2).
prim(my_reverse7/2).
prim(my_head8/2).
prim(my_last9/2).
prim(my_min_list10/2).
prim(my_min_list11/2).
prim(my_last12/2).
prim(my_pred13/2).
prim(my_reverse14/2).
prim(my_tail15/2).
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
p([['g','d','x'],['r','t','d','i'],['h','u','l','v']],[['g','d'],['r','t','d'],['h','u','l']]).
p([['k','v','b'],['k','a','a'],['t','s','g','q']],[['k','v'],['k','a'],['t','s','g']]).
