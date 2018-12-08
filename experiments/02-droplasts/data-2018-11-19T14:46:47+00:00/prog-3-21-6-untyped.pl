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
my_sumlist1(A,B):-sumlist(A,B).
my_last2(A,B):-last(A,B).
my_reverse3(A,B):-reverse(A,B).
my_head4([H|_],H).
my_last5(A,B):-last(A,B).
my_reverse6(A,B):-reverse(A,B).
my_min_list7(A,B):-min_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_len9(A,B):-length(A,B).
my_len10(A,B):-length(A,B).
my_succ11(A,B):-succ(A,B).
my_tail12([_|TL],TL).
my_tail13([_|TL],TL).
my_sumlist14(A,B):-sumlist(A,B).
my_tail15([_|TL],TL).
my_len16(A,B):-length(A,B).
my_succ17(A,B):-succ(A,B).
my_len18(A,B):-length(A,B).
my_pred19(A,B):-succ(B,A).
my_len20(A,B):-length(A,B).
prim(my_max_list0/2).
prim(my_sumlist1/2).
prim(my_last2/2).
prim(my_reverse3/2).
prim(my_head4/2).
prim(my_last5/2).
prim(my_reverse6/2).
prim(my_min_list7/2).
prim(my_sumlist8/2).
prim(my_len9/2).
prim(my_len10/2).
prim(my_succ11/2).
prim(my_tail12/2).
prim(my_tail13/2).
prim(my_sumlist14/2).
prim(my_tail15/2).
prim(my_len16/2).
prim(my_succ17/2).
prim(my_len18/2).
prim(my_pred19/2).
prim(my_len20/2).
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
p([['l','s','n','d'],['k','l','e','q'],['d','c','i','s']],[['l','s','n'],['k','l','e'],['d','c','i']]).
p([['k','w','v'],['d','c','r']],[['k','w'],['d','c']]).
