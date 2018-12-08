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
my_last0(A,B):-last(A,B).
my_head1([H|_],H).
my_max_list2(A,B):-max_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_head4([H|_],H).
my_last5(A,B):-last(A,B).
my_len6(A,B):-length(A,B).
my_min_list7(A,B):-min_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
my_head10([H|_],H).
my_pred11(A,B):-succ(B,A).
my_max_list12(A,B):-max_list(A,B).
my_tail13([_|TL],TL).
my_max_list14(A,B):-max_list(A,B).
my_max_list15(A,B):-max_list(A,B).
my_head16([H|_],H).
my_succ17(A,B):-succ(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_reverse19(A,B):-reverse(A,B).
my_sumlist20(A,B):-sumlist(A,B).
my_min_list21(A,B):-min_list(A,B).
prim(my_last0/2).
prim(my_head1/2).
prim(my_max_list2/2).
prim(my_max_list3/2).
prim(my_head4/2).
prim(my_last5/2).
prim(my_len6/2).
prim(my_min_list7/2).
prim(my_sumlist8/2).
prim(my_last9/2).
prim(my_head10/2).
prim(my_pred11/2).
prim(my_max_list12/2).
prim(my_tail13/2).
prim(my_max_list14/2).
prim(my_max_list15/2).
prim(my_head16/2).
prim(my_succ17/2).
prim(my_sumlist18/2).
prim(my_reverse19/2).
prim(my_sumlist20/2).
prim(my_min_list21/2).
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
p([['v','b','i','w'],['e','e','n','t'],['c','b','f','n']],[['v','b','i'],['e','e','n'],['c','b','f']]).
p([['e','q','y'],['s','w','s','k'],['w','u','f','l'],['e','l','l']],[['e','q'],['s','w','s'],['w','u','f'],['e','l']]).
