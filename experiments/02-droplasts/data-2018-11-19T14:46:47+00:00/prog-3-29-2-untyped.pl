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
my_pred0(A,B):-succ(B,A).
my_reverse1(A,B):-reverse(A,B).
my_tail2([_|TL],TL).
my_max_list3(A,B):-max_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_pred9(A,B):-succ(B,A).
my_head10([H|_],H).
my_succ11(A,B):-succ(A,B).
my_len12(A,B):-length(A,B).
my_head13([H|_],H).
my_reverse14(A,B):-reverse(A,B).
my_tail15([_|TL],TL).
my_succ16(A,B):-succ(A,B).
my_tail17([_|TL],TL).
my_len18(A,B):-length(A,B).
my_min_list19(A,B):-min_list(A,B).
my_head20([H|_],H).
my_sumlist21(A,B):-sumlist(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_last23(A,B):-last(A,B).
my_last24(A,B):-last(A,B).
my_len25(A,B):-length(A,B).
my_sumlist26(A,B):-sumlist(A,B).
my_succ27(A,B):-succ(A,B).
my_succ28(A,B):-succ(A,B).
prim(my_pred0/2).
prim(my_reverse1/2).
prim(my_tail2/2).
prim(my_max_list3/2).
prim(my_max_list4/2).
prim(my_min_list5/2).
prim(my_tail6/2).
prim(my_sumlist7/2).
prim(my_len8/2).
prim(my_pred9/2).
prim(my_head10/2).
prim(my_succ11/2).
prim(my_len12/2).
prim(my_head13/2).
prim(my_reverse14/2).
prim(my_tail15/2).
prim(my_succ16/2).
prim(my_tail17/2).
prim(my_len18/2).
prim(my_min_list19/2).
prim(my_head20/2).
prim(my_sumlist21/2).
prim(my_sumlist22/2).
prim(my_last23/2).
prim(my_last24/2).
prim(my_len25/2).
prim(my_sumlist26/2).
prim(my_succ27/2).
prim(my_succ28/2).
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
p([['h','j','r'],['s','c','e'],['m','r','r']],[['h','j'],['s','c'],['m','r']]).
p([['x','k','n'],['q','n','e','u'],['h','p','k','y'],['j','t','v','c']],[['x','k'],['q','n','e'],['h','p','k'],['j','t','v']]).
