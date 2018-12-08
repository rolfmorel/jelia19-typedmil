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
my_succ0(A,B):-succ(A,B).
my_succ1(A,B):-succ(A,B).
my_head2([H|_],H).
my_pred3(A,B):-succ(B,A).
my_min_list4(A,B):-min_list(A,B).
my_reverse5(A,B):-reverse(A,B).
my_max_list6(A,B):-max_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B).
my_max_list9(A,B):-max_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_pred11(A,B):-succ(B,A).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_last15(A,B):-last(A,B).
my_succ16(A,B):-succ(A,B).
my_max_list17(A,B):-max_list(A,B).
my_head18([H|_],H).
my_head19([H|_],H).
my_tail20([_|TL],TL).
my_head21([H|_],H).
my_sumlist22(A,B):-sumlist(A,B).
my_reverse23(A,B):-reverse(A,B).
my_max_list24(A,B):-max_list(A,B).
my_reverse25(A,B):-reverse(A,B).
prim(my_succ0/2).
prim(my_succ1/2).
prim(my_head2/2).
prim(my_pred3/2).
prim(my_min_list4/2).
prim(my_reverse5/2).
prim(my_max_list6/2).
prim(my_reverse7/2).
prim(my_succ8/2).
prim(my_max_list9/2).
prim(my_reverse10/2).
prim(my_pred11/2).
prim(my_pred12/2).
prim(my_max_list13/2).
prim(my_sumlist14/2).
prim(my_last15/2).
prim(my_succ16/2).
prim(my_max_list17/2).
prim(my_head18/2).
prim(my_head19/2).
prim(my_tail20/2).
prim(my_head21/2).
prim(my_sumlist22/2).
prim(my_reverse23/2).
prim(my_max_list24/2).
prim(my_reverse25/2).
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
p([['e','t','g'],['r','n','y'],['x','m','f','e'],['u','i','p','n']],[['e','t'],['r','n'],['x','m','f'],['u','i','p']]).
p([['d','b','s'],['b','g','d'],['u','d','d','w'],['u','j','a','s']],[['d','b'],['b','g'],['u','d','d'],['u','j','a']]).
