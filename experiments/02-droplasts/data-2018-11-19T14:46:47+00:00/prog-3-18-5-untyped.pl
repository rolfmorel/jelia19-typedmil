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
my_len1(A,B):-length(A,B).
my_max_list2(A,B):-max_list(A,B).
my_tail3([_|TL],TL).
my_min_list4(A,B):-min_list(A,B).
my_min_list5(A,B):-min_list(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_head8([H|_],H).
my_reverse9(A,B):-reverse(A,B).
my_head10([H|_],H).
my_head11([H|_],H).
my_succ12(A,B):-succ(A,B).
my_max_list13(A,B):-max_list(A,B).
my_last14(A,B):-last(A,B).
my_len15(A,B):-length(A,B).
my_head16([H|_],H).
my_len17(A,B):-length(A,B).
prim(my_succ0/2).
prim(my_len1/2).
prim(my_max_list2/2).
prim(my_tail3/2).
prim(my_min_list4/2).
prim(my_min_list5/2).
prim(my_sumlist6/2).
prim(my_sumlist7/2).
prim(my_head8/2).
prim(my_reverse9/2).
prim(my_head10/2).
prim(my_head11/2).
prim(my_succ12/2).
prim(my_max_list13/2).
prim(my_last14/2).
prim(my_len15/2).
prim(my_head16/2).
prim(my_len17/2).
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
p([['x','v','k','o'],['m','y','m','i'],['p','k','u'],['i','j','h']],[['x','v','k'],['m','y','m'],['p','k'],['i','j']]).
p([['v','e','g'],['u','j','w','m'],['m','m','l','j'],['r','p','h','v']],[['v','e'],['u','j','w'],['m','m','l'],['r','p','h']]).
