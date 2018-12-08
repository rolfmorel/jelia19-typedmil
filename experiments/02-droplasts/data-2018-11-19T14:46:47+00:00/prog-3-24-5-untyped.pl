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
my_last3(A,B):-last(A,B).
my_min_list4(A,B):-min_list(A,B).
my_max_list5(A,B):-max_list(A,B).
my_min_list6(A,B):-min_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_max_list8(A,B):-max_list(A,B).
my_pred9(A,B):-succ(B,A).
my_len10(A,B):-length(A,B).
my_last11(A,B):-last(A,B).
my_min_list12(A,B):-min_list(A,B).
my_len13(A,B):-length(A,B).
my_last14(A,B):-last(A,B).
my_reverse15(A,B):-reverse(A,B).
my_tail16([_|TL],TL).
my_pred17(A,B):-succ(B,A).
my_reverse18(A,B):-reverse(A,B).
my_max_list19(A,B):-max_list(A,B).
my_len20(A,B):-length(A,B).
my_len21(A,B):-length(A,B).
my_max_list22(A,B):-max_list(A,B).
my_succ23(A,B):-succ(A,B).
prim(my_sumlist0/2).
prim(my_tail1/2).
prim(my_sumlist2/2).
prim(my_last3/2).
prim(my_min_list4/2).
prim(my_max_list5/2).
prim(my_min_list6/2).
prim(my_reverse7/2).
prim(my_max_list8/2).
prim(my_pred9/2).
prim(my_len10/2).
prim(my_last11/2).
prim(my_min_list12/2).
prim(my_len13/2).
prim(my_last14/2).
prim(my_reverse15/2).
prim(my_tail16/2).
prim(my_pred17/2).
prim(my_reverse18/2).
prim(my_max_list19/2).
prim(my_len20/2).
prim(my_len21/2).
prim(my_max_list22/2).
prim(my_succ23/2).
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
p([['h','s','e'],['w','q','o']],[['h','s'],['w','q']]).
p([['d','b','i','b'],['m','l','v','o'],['q','b','l','c'],['u','o','p','e']],[['d','b','i'],['m','l','v'],['q','b','l'],['u','o','p']]).
