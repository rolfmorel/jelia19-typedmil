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
my_len1(A,B):-length(A,B).
my_tail2([_|TL],TL).
my_pred3(A,B):-succ(B,A).
my_reverse4(A,B):-reverse(A,B).
my_reverse5(A,B):-reverse(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_tail7([_|TL],TL).
my_reverse8(A,B):-reverse(A,B).
my_reverse9(A,B):-reverse(A,B).
my_pred10(A,B):-succ(B,A).
my_reverse11(A,B):-reverse(A,B).
my_reverse12(A,B):-reverse(A,B).
my_len13(A,B):-length(A,B).
my_min_list14(A,B):-min_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_pred16(A,B):-succ(B,A).
my_last17(A,B):-last(A,B).
my_max_list18(A,B):-max_list(A,B).
my_min_list19(A,B):-min_list(A,B).
my_head20([H|_],H).
my_last21(A,B):-last(A,B).
my_sumlist22(A,B):-sumlist(A,B).
prim(my_len0/2).
prim(my_len1/2).
prim(my_tail2/2).
prim(my_pred3/2).
prim(my_reverse4/2).
prim(my_reverse5/2).
prim(my_sumlist6/2).
prim(my_tail7/2).
prim(my_reverse8/2).
prim(my_reverse9/2).
prim(my_pred10/2).
prim(my_reverse11/2).
prim(my_reverse12/2).
prim(my_len13/2).
prim(my_min_list14/2).
prim(my_sumlist15/2).
prim(my_pred16/2).
prim(my_last17/2).
prim(my_max_list18/2).
prim(my_min_list19/2).
prim(my_head20/2).
prim(my_last21/2).
prim(my_sumlist22/2).
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
p([['i','a','o','s'],['n','t','k','j'],['j','c','m']],[['i','a','o'],['n','t','k'],['j','c']]).
p([['q','r','k'],['w','l','l','n'],['r','y','o']],[['q','r'],['w','l','l'],['r','y']]).
