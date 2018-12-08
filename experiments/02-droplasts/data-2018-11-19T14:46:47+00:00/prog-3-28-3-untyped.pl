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
my_sumlist0(A,B):-sumlist(A,B).
my_tail1([_|TL],TL).
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_head4([H|_],H).
my_len5(A,B):-length(A,B).
my_head6([H|_],H).
my_min_list7(A,B):-min_list(A,B).
my_reverse8(A,B):-reverse(A,B).
my_head9([H|_],H).
my_sumlist10(A,B):-sumlist(A,B).
my_tail11([_|TL],TL).
my_pred12(A,B):-succ(B,A).
my_sumlist13(A,B):-sumlist(A,B).
my_head14([H|_],H).
my_len15(A,B):-length(A,B).
my_max_list16(A,B):-max_list(A,B).
my_succ17(A,B):-succ(A,B).
my_last18(A,B):-last(A,B).
my_min_list19(A,B):-min_list(A,B).
my_reverse20(A,B):-reverse(A,B).
my_succ21(A,B):-succ(A,B).
my_max_list22(A,B):-max_list(A,B).
my_max_list23(A,B):-max_list(A,B).
my_last24(A,B):-last(A,B).
my_min_list25(A,B):-min_list(A,B).
my_succ26(A,B):-succ(A,B).
my_last27(A,B):-last(A,B).
prim(my_sumlist0/2).
prim(my_tail1/2).
prim(my_sumlist2/2).
prim(my_reverse3/2).
prim(my_head4/2).
prim(my_len5/2).
prim(my_head6/2).
prim(my_min_list7/2).
prim(my_reverse8/2).
prim(my_head9/2).
prim(my_sumlist10/2).
prim(my_tail11/2).
prim(my_pred12/2).
prim(my_sumlist13/2).
prim(my_head14/2).
prim(my_len15/2).
prim(my_max_list16/2).
prim(my_succ17/2).
prim(my_last18/2).
prim(my_min_list19/2).
prim(my_reverse20/2).
prim(my_succ21/2).
prim(my_max_list22/2).
prim(my_max_list23/2).
prim(my_last24/2).
prim(my_min_list25/2).
prim(my_succ26/2).
prim(my_last27/2).
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
p([['x','c','t'],['f','t','a'],['d','c','q','j']],[['x','c'],['f','t'],['d','c','q']]).
p([['s','r','b','q'],['j','v','a'],['m','f','o'],['i','o','f','b']],[['s','r','b'],['j','v'],['m','f'],['i','o','f']]).
