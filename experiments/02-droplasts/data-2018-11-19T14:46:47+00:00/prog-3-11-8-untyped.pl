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
my_head0([H|_],H).
my_head1([H|_],H).
my_tail2([_|TL],TL).
my_min_list3(A,B):-min_list(A,B).
my_reverse4(A,B):-reverse(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_tail6([_|TL],TL).
my_pred7(A,B):-succ(B,A).
my_max_list8(A,B):-max_list(A,B).
my_max_list9(A,B):-max_list(A,B).
my_pred10(A,B):-succ(B,A).
prim(my_head0/2).
prim(my_head1/2).
prim(my_tail2/2).
prim(my_min_list3/2).
prim(my_reverse4/2).
prim(my_sumlist5/2).
prim(my_tail6/2).
prim(my_pred7/2).
prim(my_max_list8/2).
prim(my_max_list9/2).
prim(my_pred10/2).
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
p([['g','e','h','d'],['g','l','n','e'],['c','r','h','k']],[['g','e','h'],['g','l','n'],['c','r','h']]).
p([['i','o','k'],['h','v','d','k'],['d','h','u'],['h','x','c']],[['i','o'],['h','v','d'],['d','h'],['h','x']]).
