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
my_head1([H|_],H).
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_pred4(A,B):-succ(B,A).
my_min_list5(A,B):-min_list(A,B).
my_tail6([_|TL],TL).
my_len7(A,B):-length(A,B).
my_len8(A,B):-length(A,B).
my_len9(A,B):-length(A,B).
my_pred10(A,B):-succ(B,A).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_pred13(A,B):-succ(B,A).
my_pred14(A,B):-succ(B,A).
my_reverse15(A,B):-reverse(A,B).
my_len16(A,B):-length(A,B).
my_reverse17(A,B):-reverse(A,B).
my_reverse18(A,B):-reverse(A,B).
my_pred19(A,B):-succ(B,A).
prim(my_max_list0/2).
prim(my_head1/2).
prim(my_min_list2/2).
prim(my_tail3/2).
prim(my_pred4/2).
prim(my_min_list5/2).
prim(my_tail6/2).
prim(my_len7/2).
prim(my_len8/2).
prim(my_len9/2).
prim(my_pred10/2).
prim(my_sumlist11/2).
prim(my_last12/2).
prim(my_pred13/2).
prim(my_pred14/2).
prim(my_reverse15/2).
prim(my_len16/2).
prim(my_reverse17/2).
prim(my_reverse18/2).
prim(my_pred19/2).
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
p([['g','u','o','p'],['n','v','a','y'],['o','r','b','w']],[['g','u','o'],['n','v','a'],['o','r','b']]).
p([['y','e','g'],['o','h','b'],['a','f','n','g']],[['y','e'],['o','h'],['a','f','n']]).
