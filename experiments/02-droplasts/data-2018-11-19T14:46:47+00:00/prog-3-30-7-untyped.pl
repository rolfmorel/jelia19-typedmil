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
my_last1(A,B):-last(A,B).
my_sumlist2(A,B):-sumlist(A,B).
my_len3(A,B):-length(A,B).
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_head6([H|_],H).
my_len7(A,B):-length(A,B).
my_len8(A,B):-length(A,B).
my_last9(A,B):-last(A,B).
my_tail10([_|TL],TL).
my_tail11([_|TL],TL).
my_pred12(A,B):-succ(B,A).
my_tail13([_|TL],TL).
my_max_list14(A,B):-max_list(A,B).
my_tail15([_|TL],TL).
my_head16([H|_],H).
my_tail17([_|TL],TL).
my_head18([H|_],H).
my_len19(A,B):-length(A,B).
my_pred20(A,B):-succ(B,A).
my_len21(A,B):-length(A,B).
my_len22(A,B):-length(A,B).
my_min_list23(A,B):-min_list(A,B).
my_max_list24(A,B):-max_list(A,B).
my_head25([H|_],H).
my_reverse26(A,B):-reverse(A,B).
my_max_list27(A,B):-max_list(A,B).
my_tail28([_|TL],TL).
my_head29([H|_],H).
prim(my_head0/2).
prim(my_last1/2).
prim(my_sumlist2/2).
prim(my_len3/2).
prim(my_min_list4/2).
prim(my_sumlist5/2).
prim(my_head6/2).
prim(my_len7/2).
prim(my_len8/2).
prim(my_last9/2).
prim(my_tail10/2).
prim(my_tail11/2).
prim(my_pred12/2).
prim(my_tail13/2).
prim(my_max_list14/2).
prim(my_tail15/2).
prim(my_head16/2).
prim(my_tail17/2).
prim(my_head18/2).
prim(my_len19/2).
prim(my_pred20/2).
prim(my_len21/2).
prim(my_len22/2).
prim(my_min_list23/2).
prim(my_max_list24/2).
prim(my_head25/2).
prim(my_reverse26/2).
prim(my_max_list27/2).
prim(my_tail28/2).
prim(my_head29/2).
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
p([['j','k','w'],['g','i','w','f']],[['j','k'],['g','i','w']]).
p([['p','s','d','d'],['h','j','n','r'],['u','g','p','n'],['c','g','p']],[['p','s','d'],['h','j','n'],['u','g','p'],['c','g']]).
