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
my_head1([H|_],H).
my_reverse2(A,B):-reverse(A,B).
my_sumlist3(A,B):-sumlist(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_head5([H|_],H).
my_reverse6(A,B):-reverse(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_head8([H|_],H).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_tail11([_|TL],TL).
my_sumlist12(A,B):-sumlist(A,B).
my_len13(A,B):-length(A,B).
my_last14(A,B):-last(A,B).
my_last15(A,B):-last(A,B).
my_head16([H|_],H).
my_reverse17(A,B):-reverse(A,B).
my_reverse18(A,B):-reverse(A,B).
my_min_list19(A,B):-min_list(A,B).
my_min_list20(A,B):-min_list(A,B).
my_max_list21(A,B):-max_list(A,B).
my_min_list22(A,B):-min_list(A,B).
my_len23(A,B):-length(A,B).
my_min_list24(A,B):-min_list(A,B).
my_len25(A,B):-length(A,B).
my_max_list26(A,B):-max_list(A,B).
prim(my_sumlist0/2).
prim(my_head1/2).
prim(my_reverse2/2).
prim(my_sumlist3/2).
prim(my_sumlist4/2).
prim(my_head5/2).
prim(my_reverse6/2).
prim(my_sumlist7/2).
prim(my_head8/2).
prim(my_min_list9/2).
prim(my_reverse10/2).
prim(my_tail11/2).
prim(my_sumlist12/2).
prim(my_len13/2).
prim(my_last14/2).
prim(my_last15/2).
prim(my_head16/2).
prim(my_reverse17/2).
prim(my_reverse18/2).
prim(my_min_list19/2).
prim(my_min_list20/2).
prim(my_max_list21/2).
prim(my_min_list22/2).
prim(my_len23/2).
prim(my_min_list24/2).
prim(my_len25/2).
prim(my_max_list26/2).
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
p([['j','m','f'],['g','o','m','t'],['k','f','a','b']],[['j','m'],['g','o','m'],['k','f','a']]).
p([['u','m','c','x'],['d','v','k','n'],['o','r','x'],['q','x','c','a']],[['u','m','c'],['d','v','k'],['o','r'],['q','x','c']]).
