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
my_min_list0(A,B):-min_list(A,B).
my_reverse1(A,B):-reverse(A,B).
my_last2(A,B):-last(A,B).
my_max_list3(A,B):-max_list(A,B).
my_min_list4(A,B):-min_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_reverse8(A,B):-reverse(A,B).
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A).
my_reverse11(A,B):-reverse(A,B).
my_head12([H|_],H).
my_min_list13(A,B):-min_list(A,B).
my_len14(A,B):-length(A,B).
my_pred15(A,B):-succ(B,A).
my_succ16(A,B):-succ(A,B).
my_succ17(A,B):-succ(A,B).
my_min_list18(A,B):-min_list(A,B).
my_reverse19(A,B):-reverse(A,B).
my_len20(A,B):-length(A,B).
my_tail21([_|TL],TL).
my_len22(A,B):-length(A,B).
my_pred23(A,B):-succ(B,A).
my_len24(A,B):-length(A,B).
my_pred25(A,B):-succ(B,A).
my_reverse26(A,B):-reverse(A,B).
prim(my_min_list0/2).
prim(my_reverse1/2).
prim(my_last2/2).
prim(my_max_list3/2).
prim(my_min_list4/2).
prim(my_min_list5/2).
prim(my_len6/2).
prim(my_sumlist7/2).
prim(my_reverse8/2).
prim(my_last9/2).
prim(my_pred10/2).
prim(my_reverse11/2).
prim(my_head12/2).
prim(my_min_list13/2).
prim(my_len14/2).
prim(my_pred15/2).
prim(my_succ16/2).
prim(my_succ17/2).
prim(my_min_list18/2).
prim(my_reverse19/2).
prim(my_len20/2).
prim(my_tail21/2).
prim(my_len22/2).
prim(my_pred23/2).
prim(my_len24/2).
prim(my_pred25/2).
prim(my_reverse26/2).
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
p([['t','p','g','i'],['w','x','y','b']],[['t','p','g'],['w','x','y']]).
p([['x','x','p'],['x','o','r','f'],['h','b','b','d']],[['x','x'],['x','o','r'],['h','b','b']]).
