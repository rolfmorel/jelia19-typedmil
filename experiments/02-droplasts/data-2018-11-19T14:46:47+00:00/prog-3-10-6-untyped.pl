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
my_head1([H|_],H).
my_succ2(A,B):-succ(A,B).
my_reverse3(A,B):-reverse(A,B).
my_tail4([_|TL],TL).
my_tail5([_|TL],TL).
my_reverse6(A,B):-reverse(A,B).
my_reverse7(A,B):-reverse(A,B).
my_min_list8(A,B):-min_list(A,B).
my_max_list9(A,B):-max_list(A,B).
prim(my_succ0/2).
prim(my_head1/2).
prim(my_succ2/2).
prim(my_reverse3/2).
prim(my_tail4/2).
prim(my_tail5/2).
prim(my_reverse6/2).
prim(my_reverse7/2).
prim(my_min_list8/2).
prim(my_max_list9/2).
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
p([['g','m','a','b'],['o','m','a'],['g','t','e']],[['g','m','a'],['o','m'],['g','t']]).
p([['s','n','c'],['p','y','m'],['f','m','t'],['a','x','p']],[['s','n'],['p','y'],['f','m'],['a','x']]).
