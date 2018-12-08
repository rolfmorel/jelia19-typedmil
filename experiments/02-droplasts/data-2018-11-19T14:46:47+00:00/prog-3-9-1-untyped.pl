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
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_pred2(A,B):-succ(B,A).
my_min_list3(A,B):-min_list(A,B).
my_max_list4(A,B):-max_list(A,B).
my_pred5(A,B):-succ(B,A).
my_max_list6(A,B):-max_list(A,B).
my_len7(A,B):-length(A,B).
my_reverse8(A,B):-reverse(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_pred2/2).
prim(my_min_list3/2).
prim(my_max_list4/2).
prim(my_pred5/2).
prim(my_max_list6/2).
prim(my_len7/2).
prim(my_reverse8/2).
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
p([['q','s','a','c'],['u','f','d','w'],['w','j','q','j']],[['q','s','a'],['u','f','d'],['w','j','q']]).
p([['t','v','d','n'],['m','j','p'],['o','e','v','s'],['e','x','o','o']],[['t','v','d'],['m','j'],['o','e','v'],['e','x','o']]).
