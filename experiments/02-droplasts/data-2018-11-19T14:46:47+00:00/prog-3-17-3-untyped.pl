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
my_sumlist1(A,B):-sumlist(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_len4(A,B):-length(A,B).
my_len5(A,B):-length(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_last8(A,B):-last(A,B).
my_reverse9(A,B):-reverse(A,B).
my_succ10(A,B):-succ(A,B).
my_reverse11(A,B):-reverse(A,B).
my_tail12([_|TL],TL).
my_tail13([_|TL],TL).
my_succ14(A,B):-succ(A,B).
my_reverse15(A,B):-reverse(A,B).
my_succ16(A,B):-succ(A,B).
prim(my_head0/2).
prim(my_sumlist1/2).
prim(my_sumlist2/2).
prim(my_reverse3/2).
prim(my_len4/2).
prim(my_len5/2).
prim(my_sumlist6/2).
prim(my_sumlist7/2).
prim(my_last8/2).
prim(my_reverse9/2).
prim(my_succ10/2).
prim(my_reverse11/2).
prim(my_tail12/2).
prim(my_tail13/2).
prim(my_succ14/2).
prim(my_reverse15/2).
prim(my_succ16/2).
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
p([['o','d','v','h'],['m','l','y','u'],['c','m','q']],[['o','d','v'],['m','l','y'],['c','m']]).
p([['h','w','o'],['u','e','o'],['p','e','y']],[['h','w'],['u','e'],['p','e']]).
