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
my_reverse0(A,B):-reverse(A,B).
my_max_list1(A,B):-max_list(A,B).
my_head2([H|_],H).
my_sumlist3(A,B):-sumlist(A,B).
my_head4([H|_],H).
my_last5(A,B):-last(A,B).
prim(my_reverse0/2).
prim(my_max_list1/2).
prim(my_head2/2).
prim(my_sumlist3/2).
prim(my_head4/2).
prim(my_last5/2).
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
p([['j','q','u'],['e','i','o','k'],['c','y','x','l'],['o','g','p','r']],[['j','q'],['e','i','o'],['c','y','x'],['o','g','p']]).
p([['n','x','k','d'],['y','y','v','t'],['f','p','x']],[['n','x','k'],['y','y','v'],['f','p']]).
