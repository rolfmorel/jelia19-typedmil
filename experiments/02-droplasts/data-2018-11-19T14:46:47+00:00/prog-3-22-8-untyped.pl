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
my_min_list0(A,B):-min_list(A,B).
my_tail1([_|TL],TL).
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_sumlist4(A,B):-sumlist(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
my_len7(A,B):-length(A,B).
my_last8(A,B):-last(A,B).
my_min_list9(A,B):-min_list(A,B).
my_reverse10(A,B):-reverse(A,B).
my_len11(A,B):-length(A,B).
my_min_list12(A,B):-min_list(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_max_list14(A,B):-max_list(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_min_list16(A,B):-min_list(A,B).
my_tail17([_|TL],TL).
my_len18(A,B):-length(A,B).
my_head19([H|_],H).
my_pred20(A,B):-succ(B,A).
my_sumlist21(A,B):-sumlist(A,B).
prim(my_min_list0/2).
prim(my_tail1/2).
prim(my_min_list2/2).
prim(my_tail3/2).
prim(my_sumlist4/2).
prim(my_sumlist5/2).
prim(my_min_list6/2).
prim(my_len7/2).
prim(my_last8/2).
prim(my_min_list9/2).
prim(my_reverse10/2).
prim(my_len11/2).
prim(my_min_list12/2).
prim(my_sumlist13/2).
prim(my_max_list14/2).
prim(my_sumlist15/2).
prim(my_min_list16/2).
prim(my_tail17/2).
prim(my_len18/2).
prim(my_head19/2).
prim(my_pred20/2).
prim(my_sumlist21/2).
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
p([['s','u','m','y'],['a','u','r','n'],['v','s','c','x'],['x','m','k']],[['s','u','m'],['a','u','r'],['v','s','c'],['x','m']]).
p([['d','v','t','e'],['f','t','w']],[['d','v','t'],['f','t']]).
