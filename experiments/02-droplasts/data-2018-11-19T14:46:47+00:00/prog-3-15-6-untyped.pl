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
my_succ2(A,B):-succ(A,B).
my_min_list3(A,B):-min_list(A,B).
my_tail4([_|TL],TL).
my_min_list5(A,B):-min_list(A,B).
my_last6(A,B):-last(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_len8(A,B):-length(A,B).
my_last9(A,B):-last(A,B).
my_succ10(A,B):-succ(A,B).
my_head11([H|_],H).
my_pred12(A,B):-succ(B,A).
my_max_list13(A,B):-max_list(A,B).
my_reverse14(A,B):-reverse(A,B).
prim(my_succ0/2).
prim(my_tail1/2).
prim(my_succ2/2).
prim(my_min_list3/2).
prim(my_tail4/2).
prim(my_min_list5/2).
prim(my_last6/2).
prim(my_sumlist7/2).
prim(my_len8/2).
prim(my_last9/2).
prim(my_succ10/2).
prim(my_head11/2).
prim(my_pred12/2).
prim(my_max_list13/2).
prim(my_reverse14/2).
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
p([['u','m','j'],['h','f','a'],['w','q','y'],['b','g','p']],[['u','m'],['h','f'],['w','q'],['b','g']]).
p([['i','b','u'],['s','l','a','x'],['q','f','i'],['w','j','y','h']],[['i','b'],['s','l','a'],['q','f'],['w','j','y']]).
