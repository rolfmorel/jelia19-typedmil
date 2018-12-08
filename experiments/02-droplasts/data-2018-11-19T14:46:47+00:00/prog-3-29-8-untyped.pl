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
my_reverse1(A,B):-reverse(A,B).
my_succ2(A,B):-succ(A,B).
my_max_list3(A,B):-max_list(A,B).
my_last4(A,B):-last(A,B).
my_last5(A,B):-last(A,B).
my_head6([H|_],H).
my_tail7([_|TL],TL).
my_head8([H|_],H).
my_last9(A,B):-last(A,B).
my_len10(A,B):-length(A,B).
my_succ11(A,B):-succ(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_pred16(A,B):-succ(B,A).
my_succ17(A,B):-succ(A,B).
my_tail18([_|TL],TL).
my_min_list19(A,B):-min_list(A,B).
my_min_list20(A,B):-min_list(A,B).
my_max_list21(A,B):-max_list(A,B).
my_reverse22(A,B):-reverse(A,B).
my_max_list23(A,B):-max_list(A,B).
my_len24(A,B):-length(A,B).
my_reverse25(A,B):-reverse(A,B).
my_last26(A,B):-last(A,B).
my_min_list27(A,B):-min_list(A,B).
my_head28([H|_],H).
prim(my_last0/2).
prim(my_reverse1/2).
prim(my_succ2/2).
prim(my_max_list3/2).
prim(my_last4/2).
prim(my_last5/2).
prim(my_head6/2).
prim(my_tail7/2).
prim(my_head8/2).
prim(my_last9/2).
prim(my_len10/2).
prim(my_succ11/2).
prim(my_sumlist12/2).
prim(my_sumlist13/2).
prim(my_sumlist14/2).
prim(my_sumlist15/2).
prim(my_pred16/2).
prim(my_succ17/2).
prim(my_tail18/2).
prim(my_min_list19/2).
prim(my_min_list20/2).
prim(my_max_list21/2).
prim(my_reverse22/2).
prim(my_max_list23/2).
prim(my_len24/2).
prim(my_reverse25/2).
prim(my_last26/2).
prim(my_min_list27/2).
prim(my_head28/2).
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
p([['y','s','t'],['y','c','f','q'],['l','s','o','p']],[['y','s'],['y','c','f'],['l','s','o']]).
p([['y','g','h','w'],['o','e','o']],[['y','g','h'],['o','e']]).
