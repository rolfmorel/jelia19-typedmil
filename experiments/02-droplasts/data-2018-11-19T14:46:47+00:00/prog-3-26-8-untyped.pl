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
my_tail1([_|TL],TL).
my_len2(A,B):-length(A,B).
my_last3(A,B):-last(A,B).
my_last4(A,B):-last(A,B).
my_min_list5(A,B):-min_list(A,B).
my_pred6(A,B):-succ(B,A).
my_max_list7(A,B):-max_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_max_list9(A,B):-max_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_succ11(A,B):-succ(A,B).
my_tail12([_|TL],TL).
my_sumlist13(A,B):-sumlist(A,B).
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_head16([H|_],H).
my_reverse17(A,B):-reverse(A,B).
my_reverse18(A,B):-reverse(A,B).
my_max_list19(A,B):-max_list(A,B).
my_max_list20(A,B):-max_list(A,B).
my_len21(A,B):-length(A,B).
my_pred22(A,B):-succ(B,A).
my_reverse23(A,B):-reverse(A,B).
my_succ24(A,B):-succ(A,B).
my_min_list25(A,B):-min_list(A,B).
prim(my_succ0/2).
prim(my_tail1/2).
prim(my_len2/2).
prim(my_last3/2).
prim(my_last4/2).
prim(my_min_list5/2).
prim(my_pred6/2).
prim(my_max_list7/2).
prim(my_min_list8/2).
prim(my_max_list9/2).
prim(my_max_list10/2).
prim(my_succ11/2).
prim(my_tail12/2).
prim(my_sumlist13/2).
prim(my_len14/2).
prim(my_last15/2).
prim(my_head16/2).
prim(my_reverse17/2).
prim(my_reverse18/2).
prim(my_max_list19/2).
prim(my_max_list20/2).
prim(my_len21/2).
prim(my_pred22/2).
prim(my_reverse23/2).
prim(my_succ24/2).
prim(my_min_list25/2).
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
p([['v','j','j'],['p','a','a'],['g','o','n']],[['v','j'],['p','a'],['g','o']]).
p([['i','k','g','k'],['v','u','l']],[['i','k','g'],['v','u']]).
