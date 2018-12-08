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
my_succ1(A,B):-succ(A,B).
my_pred2(A,B):-succ(B,A).
my_pred3(A,B):-succ(B,A).
my_reverse4(A,B):-reverse(A,B).
my_min_list5(A,B):-min_list(A,B).
my_reverse6(A,B):-reverse(A,B).
my_reverse7(A,B):-reverse(A,B).
my_tail8([_|TL],TL).
my_tail9([_|TL],TL).
my_succ10(A,B):-succ(A,B).
my_tail11([_|TL],TL).
my_min_list12(A,B):-min_list(A,B).
my_max_list13(A,B):-max_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_last15(A,B):-last(A,B).
my_head16([H|_],H).
my_pred17(A,B):-succ(B,A).
my_tail18([_|TL],TL).
my_min_list19(A,B):-min_list(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_reverse21(A,B):-reverse(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_min_list23(A,B):-min_list(A,B).
prim(my_min_list0/2).
prim(my_succ1/2).
prim(my_pred2/2).
prim(my_pred3/2).
prim(my_reverse4/2).
prim(my_min_list5/2).
prim(my_reverse6/2).
prim(my_reverse7/2).
prim(my_tail8/2).
prim(my_tail9/2).
prim(my_succ10/2).
prim(my_tail11/2).
prim(my_min_list12/2).
prim(my_max_list13/2).
prim(my_reverse14/2).
prim(my_last15/2).
prim(my_head16/2).
prim(my_pred17/2).
prim(my_tail18/2).
prim(my_min_list19/2).
prim(my_sumlist20/2).
prim(my_reverse21/2).
prim(my_sumlist22/2).
prim(my_min_list23/2).
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
p([['c','j','i'],['l','t','q','g']],[['c','j'],['l','t','q']]).
p([['m','d','p','q'],['e','r','x','h'],['j','a','d','q']],[['m','d','p'],['e','r','x'],['j','a','d']]).
