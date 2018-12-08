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
my_reverse0(A,B):-reverse(A,B).
my_last1(A,B):-last(A,B).
my_min_list2(A,B):-min_list(A,B).
my_succ3(A,B):-succ(A,B).
my_head4([H|_],H).
my_reverse5(A,B):-reverse(A,B).
my_tail6([_|TL],TL).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
my_reverse9(A,B):-reverse(A,B).
my_min_list10(A,B):-min_list(A,B).
my_reverse11(A,B):-reverse(A,B).
my_last12(A,B):-last(A,B).
my_tail13([_|TL],TL).
my_sumlist14(A,B):-sumlist(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_pred16(A,B):-succ(B,A).
my_min_list17(A,B):-min_list(A,B).
my_tail18([_|TL],TL).
my_max_list19(A,B):-max_list(A,B).
my_head20([H|_],H).
my_tail21([_|TL],TL).
my_tail22([_|TL],TL).
my_head23([H|_],H).
my_max_list24(A,B):-max_list(A,B).
my_len25(A,B):-length(A,B).
prim(my_reverse0/2).
prim(my_last1/2).
prim(my_min_list2/2).
prim(my_succ3/2).
prim(my_head4/2).
prim(my_reverse5/2).
prim(my_tail6/2).
prim(my_reverse7/2).
prim(my_last8/2).
prim(my_reverse9/2).
prim(my_min_list10/2).
prim(my_reverse11/2).
prim(my_last12/2).
prim(my_tail13/2).
prim(my_sumlist14/2).
prim(my_sumlist15/2).
prim(my_pred16/2).
prim(my_min_list17/2).
prim(my_tail18/2).
prim(my_max_list19/2).
prim(my_head20/2).
prim(my_tail21/2).
prim(my_tail22/2).
prim(my_head23/2).
prim(my_max_list24/2).
prim(my_len25/2).
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
p([['v','c','u','k'],['l','r','q','l'],['t','b','c','t']],[['v','c','u'],['l','r','q'],['t','b','c']]).
p([['f','j','l','a'],['m','x','n','f'],['x','m','d'],['a','q','o']],[['f','j','l'],['m','x','n'],['x','m'],['a','q']]).
