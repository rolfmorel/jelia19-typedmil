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
my_len0(A,B):-length(A,B).
my_pred1(A,B):-succ(B,A).
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_succ8(A,B):-succ(A,B).
my_pred9(A,B):-succ(B,A).
my_reverse10(A,B):-reverse(A,B).
my_last11(A,B):-last(A,B).
my_succ12(A,B):-succ(A,B).
my_last13(A,B):-last(A,B).
prim(my_len0/2).
prim(my_pred1/2).
prim(my_sumlist2/2).
prim(my_reverse3/2).
prim(my_min_list4/2).
prim(my_sumlist5/2).
prim(my_sumlist6/2).
prim(my_sumlist7/2).
prim(my_succ8/2).
prim(my_pred9/2).
prim(my_reverse10/2).
prim(my_last11/2).
prim(my_succ12/2).
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
p([['x','g','g','u'],['s','m','x','s'],['j','f','o','y']],[['x','g','g'],['s','m','x'],['j','f','o']]).
p([['u','h','t','q'],['w','q','d'],['k','i','r','x'],['n','u','m','m']],[['u','h','t'],['w','q'],['k','i','r'],['n','u','m']]).
