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
my_last1(A,B):-last(A,B).
my_reverse2(A,B):-reverse(A,B).
my_succ3(A,B):-succ(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_reverse6(A,B):-reverse(A,B).
my_len7(A,B):-length(A,B).
my_head8([H|_],H).
my_reverse9(A,B):-reverse(A,B).
prim(my_tail0/2).
prim(my_last1/2).
prim(my_reverse2/2).
prim(my_succ3/2).
prim(my_max_list4/2).
prim(my_len5/2).
prim(my_reverse6/2).
prim(my_len7/2).
prim(my_head8/2).
prim(my_reverse9/2).
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
p([['q','s','f','p'],['j','b','h','p']],[['q','s','f'],['j','b','h']]).
p([['q','k','k','v'],['l','f','p','v'],['p','u','n','f']],[['q','k','k'],['l','f','p'],['p','u','n']]).
