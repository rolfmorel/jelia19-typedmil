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
my_len0(A,B):-length(A,B).
my_last1(A,B):-last(A,B).
my_pred2(A,B):-succ(B,A).
my_last3(A,B):-last(A,B).
my_last4(A,B):-last(A,B).
my_reverse5(A,B):-reverse(A,B).
my_succ6(A,B):-succ(A,B).
my_last7(A,B):-last(A,B).
my_last8(A,B):-last(A,B).
my_last9(A,B):-last(A,B).
my_max_list10(A,B):-max_list(A,B).
my_min_list11(A,B):-min_list(A,B).
my_tail12([_|TL],TL).
my_reverse13(A,B):-reverse(A,B).
my_last14(A,B):-last(A,B).
my_tail15([_|TL],TL).
my_len16(A,B):-length(A,B).
my_succ17(A,B):-succ(A,B).
my_min_list18(A,B):-min_list(A,B).
my_max_list19(A,B):-max_list(A,B).
my_reverse20(A,B):-reverse(A,B).
my_last21(A,B):-last(A,B).
my_head22([H|_],H).
my_min_list23(A,B):-min_list(A,B).
my_sumlist24(A,B):-sumlist(A,B).
my_reverse25(A,B):-reverse(A,B).
prim(my_len0/2).
prim(my_last1/2).
prim(my_pred2/2).
prim(my_last3/2).
prim(my_last4/2).
prim(my_reverse5/2).
prim(my_succ6/2).
prim(my_last7/2).
prim(my_last8/2).
prim(my_last9/2).
prim(my_max_list10/2).
prim(my_min_list11/2).
prim(my_tail12/2).
prim(my_reverse13/2).
prim(my_last14/2).
prim(my_tail15/2).
prim(my_len16/2).
prim(my_succ17/2).
prim(my_min_list18/2).
prim(my_max_list19/2).
prim(my_reverse20/2).
prim(my_last21/2).
prim(my_head22/2).
prim(my_min_list23/2).
prim(my_sumlist24/2).
prim(my_reverse25/2).
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
p([['g','f','j','a'],['u','h','r'],['g','m','q'],['x','u','f']],[['g','f','j'],['u','h'],['g','m'],['x','u']]).
p([['f','i','l'],['c','l','e','w'],['r','t','e','u']],[['f','i'],['c','l','e'],['r','t','e']]).
