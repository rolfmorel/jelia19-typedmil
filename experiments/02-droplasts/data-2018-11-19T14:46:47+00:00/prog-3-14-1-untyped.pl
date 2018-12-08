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
my_last1(A,B):-last(A,B).
my_succ2(A,B):-succ(A,B).
my_tail3([_|TL],TL).
my_min_list4(A,B):-min_list(A,B).
my_head5([H|_],H).
my_head6([H|_],H).
my_len7(A,B):-length(A,B).
my_tail8([_|TL],TL).
my_max_list9(A,B):-max_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_head11([H|_],H).
my_sumlist12(A,B):-sumlist(A,B).
my_last13(A,B):-last(A,B).
prim(my_min_list0/2).
prim(my_last1/2).
prim(my_succ2/2).
prim(my_tail3/2).
prim(my_min_list4/2).
prim(my_head5/2).
prim(my_head6/2).
prim(my_len7/2).
prim(my_tail8/2).
prim(my_max_list9/2).
prim(my_max_list10/2).
prim(my_head11/2).
prim(my_sumlist12/2).
prim(my_last13/2).
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
p([['s','h','l','n'],['h','j','j'],['f','v','p','s'],['a','s','f','g']],[['s','h','l'],['h','j'],['f','v','p'],['a','s','f']]).
p([['m','u','r','q'],['v','e','t']],[['m','u','r'],['v','e']]).
