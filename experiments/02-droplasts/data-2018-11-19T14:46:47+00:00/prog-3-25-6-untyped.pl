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
my_max_list0(A,B):-max_list(A,B).
my_reverse1(A,B):-reverse(A,B).
my_max_list2(A,B):-max_list(A,B).
my_tail3([_|TL],TL).
my_reverse4(A,B):-reverse(A,B).
my_reverse5(A,B):-reverse(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_head7([H|_],H).
my_succ8(A,B):-succ(A,B).
my_reverse9(A,B):-reverse(A,B).
my_reverse10(A,B):-reverse(A,B).
my_reverse11(A,B):-reverse(A,B).
my_head12([H|_],H).
my_tail13([_|TL],TL).
my_min_list14(A,B):-min_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_max_list16(A,B):-max_list(A,B).
my_tail17([_|TL],TL).
my_last18(A,B):-last(A,B).
my_len19(A,B):-length(A,B).
my_last20(A,B):-last(A,B).
my_succ21(A,B):-succ(A,B).
my_pred22(A,B):-succ(B,A).
my_pred23(A,B):-succ(B,A).
my_head24([H|_],H).
prim(my_max_list0/2).
prim(my_reverse1/2).
prim(my_max_list2/2).
prim(my_tail3/2).
prim(my_reverse4/2).
prim(my_reverse5/2).
prim(my_sumlist6/2).
prim(my_head7/2).
prim(my_succ8/2).
prim(my_reverse9/2).
prim(my_reverse10/2).
prim(my_reverse11/2).
prim(my_head12/2).
prim(my_tail13/2).
prim(my_min_list14/2).
prim(my_sumlist15/2).
prim(my_max_list16/2).
prim(my_tail17/2).
prim(my_last18/2).
prim(my_len19/2).
prim(my_last20/2).
prim(my_succ21/2).
prim(my_pred22/2).
prim(my_pred23/2).
prim(my_head24/2).
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
p([['l','v','j'],['t','h','d'],['o','w','f']],[['l','v'],['t','h'],['o','w']]).
p([['a','i','l'],['t','c','f','h'],['s','o','i','t'],['o','p','t','j']],[['a','i'],['t','c','f'],['s','o','i'],['o','p','t']]).
