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
my_tail1([_|TL],TL).
my_max_list2(A,B):-max_list(A,B).
my_max_list3(A,B):-max_list(A,B).
my_tail4([_|TL],TL).
my_reverse5(A,B):-reverse(A,B).
my_len6(A,B):-length(A,B).
my_head7([H|_],H).
my_succ8(A,B):-succ(A,B).
my_succ9(A,B):-succ(A,B).
prim(my_head0/2).
prim(my_tail1/2).
prim(my_max_list2/2).
prim(my_max_list3/2).
prim(my_tail4/2).
prim(my_reverse5/2).
prim(my_len6/2).
prim(my_head7/2).
prim(my_succ8/2).
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
p([['f','q','p'],['w','j','a']],[['f','q'],['w','j']]).
p([['r','p','t'],['l','v','j','g'],['c','u','b']],[['r','p'],['l','v','j'],['c','u']]).
