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
my_tail1([_|TL],TL).
my_len2(A,B):-length(A,B).
my_last3(A,B):-last(A,B).
my_min_list4(A,B):-min_list(A,B).
my_succ5(A,B):-succ(A,B).
my_min_list6(A,B):-min_list(A,B).
my_len7(A,B):-length(A,B).
my_min_list8(A,B):-min_list(A,B).
my_len9(A,B):-length(A,B).
my_len10(A,B):-length(A,B).
my_max_list11(A,B):-max_list(A,B).
prim(my_sumlist0/2).
prim(my_tail1/2).
prim(my_len2/2).
prim(my_last3/2).
prim(my_min_list4/2).
prim(my_succ5/2).
prim(my_min_list6/2).
prim(my_len7/2).
prim(my_min_list8/2).
prim(my_len9/2).
prim(my_len10/2).
prim(my_max_list11/2).
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
p([['s','j','v'],['s','k','j','f'],['c','k','i','k']],[['s','j'],['s','k','j'],['c','k','i']]).
p([['v','y','v'],['m','x','t','w'],['r','c','m','w'],['o','t','q']],[['v','y'],['m','x','t'],['r','c','m'],['o','t']]).
