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
my_last0(A,B):-last(A,B).
my_succ1(A,B):-succ(A,B).
my_head2([H|_],H).
my_min_list3(A,B):-min_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A).
my_reverse7(A,B):-reverse(A,B).
my_len8(A,B):-length(A,B).
my_succ9(A,B):-succ(A,B).
my_max_list10(A,B):-max_list(A,B).
my_len11(A,B):-length(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_last13(A,B):-last(A,B).
my_succ14(A,B):-succ(A,B).
my_head15([H|_],H).
my_sumlist16(A,B):-sumlist(A,B).
my_head17([H|_],H).
my_max_list18(A,B):-max_list(A,B).
my_tail19([_|TL],TL).
my_sumlist20(A,B):-sumlist(A,B).
my_reverse21(A,B):-reverse(A,B).
my_last22(A,B):-last(A,B).
my_last23(A,B):-last(A,B).
my_sumlist24(A,B):-sumlist(A,B).
my_sumlist25(A,B):-sumlist(A,B).
my_pred26(A,B):-succ(B,A).
prim(my_last0/2).
prim(my_succ1/2).
prim(my_head2/2).
prim(my_min_list3/2).
prim(my_min_list4/2).
prim(my_max_list5/2).
prim(my_pred6/2).
prim(my_reverse7/2).
prim(my_len8/2).
prim(my_succ9/2).
prim(my_max_list10/2).
prim(my_len11/2).
prim(my_sumlist12/2).
prim(my_last13/2).
prim(my_succ14/2).
prim(my_head15/2).
prim(my_sumlist16/2).
prim(my_head17/2).
prim(my_max_list18/2).
prim(my_tail19/2).
prim(my_sumlist20/2).
prim(my_reverse21/2).
prim(my_last22/2).
prim(my_last23/2).
prim(my_sumlist24/2).
prim(my_sumlist25/2).
prim(my_pred26/2).
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
p([['v','v','y','v'],['w','k','j'],['h','p','i','w']],[['v','v','y'],['w','k'],['h','p','i']]).
p([['t','f','a'],['b','s','w']],[['t','f'],['b','s']]).
