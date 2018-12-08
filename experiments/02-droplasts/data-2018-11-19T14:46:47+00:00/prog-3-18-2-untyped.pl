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
my_min_list2(A,B):-min_list(A,B).
my_tail3([_|TL],TL).
my_last4(A,B):-last(A,B).
my_reverse5(A,B):-reverse(A,B).
my_succ6(A,B):-succ(A,B).
my_succ7(A,B):-succ(A,B).
my_len8(A,B):-length(A,B).
my_max_list9(A,B):-max_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_len11(A,B):-length(A,B).
my_max_list12(A,B):-max_list(A,B).
my_reverse13(A,B):-reverse(A,B).
my_tail14([_|TL],TL).
my_len15(A,B):-length(A,B).
my_tail16([_|TL],TL).
my_reverse17(A,B):-reverse(A,B).
prim(my_last0/2).
prim(my_head1/2).
prim(my_min_list2/2).
prim(my_tail3/2).
prim(my_last4/2).
prim(my_reverse5/2).
prim(my_succ6/2).
prim(my_succ7/2).
prim(my_len8/2).
prim(my_max_list9/2).
prim(my_max_list10/2).
prim(my_len11/2).
prim(my_max_list12/2).
prim(my_reverse13/2).
prim(my_tail14/2).
prim(my_len15/2).
prim(my_tail16/2).
prim(my_reverse17/2).
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
p([['x','q','c','q'],['j','r','u','o'],['d','r','f'],['w','n','h','i']],[['x','q','c'],['j','r','u'],['d','r'],['w','n','h']]).
p([['h','v','m','j'],['e','e','v']],[['h','v','m'],['e','e']]).
