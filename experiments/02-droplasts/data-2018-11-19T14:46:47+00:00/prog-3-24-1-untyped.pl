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
my_reverse1(A,B):-reverse(A,B).
my_tail2([_|TL],TL).
my_len3(A,B):-length(A,B).
my_succ4(A,B):-succ(A,B).
my_min_list5(A,B):-min_list(A,B).
my_reverse6(A,B):-reverse(A,B).
my_pred7(A,B):-succ(B,A).
my_tail8([_|TL],TL).
my_sumlist9(A,B):-sumlist(A,B).
my_len10(A,B):-length(A,B).
my_max_list11(A,B):-max_list(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_min_list13(A,B):-min_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_max_list15(A,B):-max_list(A,B).
my_min_list16(A,B):-min_list(A,B).
my_min_list17(A,B):-min_list(A,B).
my_head18([H|_],H).
my_max_list19(A,B):-max_list(A,B).
my_reverse20(A,B):-reverse(A,B).
my_sumlist21(A,B):-sumlist(A,B).
my_sumlist22(A,B):-sumlist(A,B).
my_last23(A,B):-last(A,B).
prim(my_sumlist0/2).
prim(my_reverse1/2).
prim(my_tail2/2).
prim(my_len3/2).
prim(my_succ4/2).
prim(my_min_list5/2).
prim(my_reverse6/2).
prim(my_pred7/2).
prim(my_tail8/2).
prim(my_sumlist9/2).
prim(my_len10/2).
prim(my_max_list11/2).
prim(my_sumlist12/2).
prim(my_min_list13/2).
prim(my_reverse14/2).
prim(my_max_list15/2).
prim(my_min_list16/2).
prim(my_min_list17/2).
prim(my_head18/2).
prim(my_max_list19/2).
prim(my_reverse20/2).
prim(my_sumlist21/2).
prim(my_sumlist22/2).
prim(my_last23/2).
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
p([['b','c','p','w'],['s','g','u'],['t','r','j','p']],[['b','c','p'],['s','g'],['t','r','j']]).
p([['c','q','l','t'],['e','c','e']],[['c','q','l'],['e','c']]).
