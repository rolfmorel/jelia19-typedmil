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
my_last1(A,B):-last(A,B).
my_len2(A,B):-length(A,B).
my_head3([H|_],H).
my_len4(A,B):-length(A,B).
my_last5(A,B):-last(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_len7(A,B):-length(A,B).
my_pred8(A,B):-succ(B,A).
my_last9(A,B):-last(A,B).
my_reverse10(A,B):-reverse(A,B).
my_succ11(A,B):-succ(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_reverse13(A,B):-reverse(A,B).
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
prim(my_max_list0/2).
prim(my_last1/2).
prim(my_len2/2).
prim(my_head3/2).
prim(my_len4/2).
prim(my_last5/2).
prim(my_sumlist6/2).
prim(my_len7/2).
prim(my_pred8/2).
prim(my_last9/2).
prim(my_reverse10/2).
prim(my_succ11/2).
prim(my_sumlist12/2).
prim(my_reverse13/2).
prim(my_max_list14/2).
prim(my_sumlist15/2).
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
p([['p','x','e'],['w','j','s','v'],['f','c','i','s']],[['p','x'],['w','j','s'],['f','c','i']]).
p([['m','s','a'],['s','n','i','o'],['x','j','x']],[['m','s'],['s','n','i'],['x','j']]).
