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
my_last0(A,B):-last(A,B).
my_len1(A,B):-length(A,B).
my_reverse2(A,B):-reverse(A,B).
my_tail3([_|TL],TL).
my_reverse4(A,B):-reverse(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_tail6([_|TL],TL).
my_last7(A,B):-last(A,B).
my_last8(A,B):-last(A,B).
my_len9(A,B):-length(A,B).
my_last10(A,B):-last(A,B).
my_succ11(A,B):-succ(A,B).
my_tail12([_|TL],TL).
my_tail13([_|TL],TL).
my_head14([H|_],H).
prim(my_last0/2).
prim(my_len1/2).
prim(my_reverse2/2).
prim(my_tail3/2).
prim(my_reverse4/2).
prim(my_sumlist5/2).
prim(my_tail6/2).
prim(my_last7/2).
prim(my_last8/2).
prim(my_len9/2).
prim(my_last10/2).
prim(my_succ11/2).
prim(my_tail12/2).
prim(my_tail13/2).
prim(my_head14/2).
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
p([['y','t','o'],['v','w','t'],['k','k','a'],['g','k','c','l']],[['y','t'],['v','w'],['k','k'],['g','k','c']]).
p([['d','t','g','c'],['v','d','y','f']],[['d','t','g'],['v','d','y']]).
