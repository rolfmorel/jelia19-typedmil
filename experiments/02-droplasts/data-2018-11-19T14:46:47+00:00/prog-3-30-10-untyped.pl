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
my_succ2(A,B):-succ(A,B).
my_min_list3(A,B):-min_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_last6(A,B):-last(A,B).
my_len7(A,B):-length(A,B).
my_max_list8(A,B):-max_list(A,B).
my_min_list9(A,B):-min_list(A,B).
my_head10([H|_],H).
my_min_list11(A,B):-min_list(A,B).
my_pred12(A,B):-succ(B,A).
my_succ13(A,B):-succ(A,B).
my_tail14([_|TL],TL).
my_tail15([_|TL],TL).
my_reverse16(A,B):-reverse(A,B).
my_head17([H|_],H).
my_succ18(A,B):-succ(A,B).
my_head19([H|_],H).
my_head20([H|_],H).
my_head21([H|_],H).
my_len22(A,B):-length(A,B).
my_last23(A,B):-last(A,B).
my_succ24(A,B):-succ(A,B).
my_pred25(A,B):-succ(B,A).
my_head26([H|_],H).
my_last27(A,B):-last(A,B).
my_last28(A,B):-last(A,B).
my_len29(A,B):-length(A,B).
prim(my_max_list0/2).
prim(my_sumlist1/2).
prim(my_succ2/2).
prim(my_min_list3/2).
prim(my_max_list4/2).
prim(my_len5/2).
prim(my_last6/2).
prim(my_len7/2).
prim(my_max_list8/2).
prim(my_min_list9/2).
prim(my_head10/2).
prim(my_min_list11/2).
prim(my_pred12/2).
prim(my_succ13/2).
prim(my_tail14/2).
prim(my_tail15/2).
prim(my_reverse16/2).
prim(my_head17/2).
prim(my_succ18/2).
prim(my_head19/2).
prim(my_head20/2).
prim(my_head21/2).
prim(my_len22/2).
prim(my_last23/2).
prim(my_succ24/2).
prim(my_pred25/2).
prim(my_head26/2).
prim(my_last27/2).
prim(my_last28/2).
prim(my_len29/2).
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
p([['t','r','k','k'],['f','n','t'],['g','u','g'],['t','u','g']],[['t','r','k'],['f','n'],['g','u'],['t','u']]).
p([['t','j','r','v'],['a','u','q'],['i','q','b','s']],[['t','j','r'],['a','u'],['i','q','b']]).
