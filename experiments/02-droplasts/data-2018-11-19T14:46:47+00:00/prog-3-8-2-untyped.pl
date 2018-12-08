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
my_max_list2(A,B):-max_list(A,B).
my_pred3(A,B):-succ(B,A).
my_len4(A,B):-length(A,B).
my_min_list5(A,B):-min_list(A,B).
my_head6([H|_],H).
my_max_list7(A,B):-max_list(A,B).
prim(my_sumlist0/2).
prim(my_head1/2).
prim(my_max_list2/2).
prim(my_pred3/2).
prim(my_len4/2).
prim(my_min_list5/2).
prim(my_head6/2).
prim(my_max_list7/2).
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
p([['l','t','q','n'],['d','m','g'],['x','m','s','p']],[['l','t','q'],['d','m'],['x','m','s']]).
p([['l','v','d'],['m','v','c']],[['l','v'],['m','v']]).
