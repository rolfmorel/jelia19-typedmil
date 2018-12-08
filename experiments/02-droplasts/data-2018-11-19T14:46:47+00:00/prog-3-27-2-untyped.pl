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
my_head1([H|_],H).
my_min_list2(A,B):-min_list(A,B).
my_pred3(A,B):-succ(B,A).
my_last4(A,B):-last(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B).
my_pred7(A,B):-succ(B,A).
my_reverse8(A,B):-reverse(A,B).
my_pred9(A,B):-succ(B,A).
my_reverse10(A,B):-reverse(A,B).
my_tail11([_|TL],TL).
my_max_list12(A,B):-max_list(A,B).
my_min_list13(A,B):-min_list(A,B).
my_head14([H|_],H).
my_reverse15(A,B):-reverse(A,B).
my_min_list16(A,B):-min_list(A,B).
my_max_list17(A,B):-max_list(A,B).
my_reverse18(A,B):-reverse(A,B).
my_last19(A,B):-last(A,B).
my_head20([H|_],H).
my_succ21(A,B):-succ(A,B).
my_tail22([_|TL],TL).
my_len23(A,B):-length(A,B).
my_sumlist24(A,B):-sumlist(A,B).
my_succ25(A,B):-succ(A,B).
my_reverse26(A,B):-reverse(A,B).
prim(my_succ0/2).
prim(my_head1/2).
prim(my_min_list2/2).
prim(my_pred3/2).
prim(my_last4/2).
prim(my_sumlist5/2).
prim(my_succ6/2).
prim(my_pred7/2).
prim(my_reverse8/2).
prim(my_pred9/2).
prim(my_reverse10/2).
prim(my_tail11/2).
prim(my_max_list12/2).
prim(my_min_list13/2).
prim(my_head14/2).
prim(my_reverse15/2).
prim(my_min_list16/2).
prim(my_max_list17/2).
prim(my_reverse18/2).
prim(my_last19/2).
prim(my_head20/2).
prim(my_succ21/2).
prim(my_tail22/2).
prim(my_len23/2).
prim(my_sumlist24/2).
prim(my_succ25/2).
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
p([['w','r','x'],['p','v','h','b'],['r','w','n','q']],[['w','r'],['p','v','h'],['r','w','n']]).
p([['t','u','g'],['d','e','c','r'],['i','m','c','w'],['o','d','h']],[['t','u'],['d','e','c'],['i','m','c'],['o','d']]).
