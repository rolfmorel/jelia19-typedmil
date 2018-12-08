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
my_sumlist1(A,B):-sumlist(A,B).
my_succ2(A,B):-succ(A,B).
my_reverse3(A,B):-reverse(A,B).
my_pred4(A,B):-succ(B,A).
my_head5([H|_],H).
my_pred6(A,B):-succ(B,A).
my_sumlist7(A,B):-sumlist(A,B).
my_reverse8(A,B):-reverse(A,B).
my_min_list9(A,B):-min_list(A,B).
my_tail10([_|TL],TL).
my_min_list11(A,B):-min_list(A,B).
my_min_list12(A,B):-min_list(A,B).
my_max_list13(A,B):-max_list(A,B).
my_head14([H|_],H).
my_succ15(A,B):-succ(A,B).
my_len16(A,B):-length(A,B).
my_head17([H|_],H).
my_last18(A,B):-last(A,B).
my_pred19(A,B):-succ(B,A).
my_succ20(A,B):-succ(A,B).
my_reverse21(A,B):-reverse(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_tail23([_|TL],TL).
my_len24(A,B):-length(A,B).
my_tail25([_|TL],TL).
my_len26(A,B):-length(A,B).
my_sumlist27(A,B):-sumlist(A,B).
prim(my_pred0/2).
prim(my_sumlist1/2).
prim(my_succ2/2).
prim(my_reverse3/2).
prim(my_pred4/2).
prim(my_head5/2).
prim(my_pred6/2).
prim(my_sumlist7/2).
prim(my_reverse8/2).
prim(my_min_list9/2).
prim(my_tail10/2).
prim(my_min_list11/2).
prim(my_min_list12/2).
prim(my_max_list13/2).
prim(my_head14/2).
prim(my_succ15/2).
prim(my_len16/2).
prim(my_head17/2).
prim(my_last18/2).
prim(my_pred19/2).
prim(my_succ20/2).
prim(my_reverse21/2).
prim(my_sumlist22/2).
prim(my_tail23/2).
prim(my_len24/2).
prim(my_tail25/2).
prim(my_len26/2).
prim(my_sumlist27/2).
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
p([['u','v','q','u'],['u','v','c','l'],['w','v','m']],[['u','v','q'],['u','v','c'],['w','v']]).
p([['f','k','h'],['j','l','k'],['t','q','v','y'],['a','h','w']],[['f','k'],['j','l'],['t','q','v'],['a','h']]).
