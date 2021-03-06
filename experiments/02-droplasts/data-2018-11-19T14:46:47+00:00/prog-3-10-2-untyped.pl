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
my_pred0(A,B):-succ(B,A).
my_head1([H|_],H).
my_sumlist2(A,B):-sumlist(A,B).
my_head3([H|_],H).
my_last4(A,B):-last(A,B).
my_reverse5(A,B):-reverse(A,B).
my_max_list6(A,B):-max_list(A,B).
my_len7(A,B):-length(A,B).
my_head8([H|_],H).
my_succ9(A,B):-succ(A,B).
prim(my_pred0/2).
prim(my_head1/2).
prim(my_sumlist2/2).
prim(my_head3/2).
prim(my_last4/2).
prim(my_reverse5/2).
prim(my_max_list6/2).
prim(my_len7/2).
prim(my_head8/2).
prim(my_succ9/2).
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
p([['v','p','c'],['f','h','m','a'],['h','h','k'],['y','x','a','y']],[['v','p'],['f','h','m'],['h','h'],['y','x','a']]).
p([['p','x','i'],['h','x','b','p'],['l','k','u','p']],[['p','x'],['h','x','b'],['l','k','u']]).
