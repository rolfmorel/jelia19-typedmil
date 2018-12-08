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
my_max_list0(A,B):-max_list(A,B).
my_tail1([_|TL],TL).
my_sumlist2(A,B):-sumlist(A,B).
my_reverse3(A,B):-reverse(A,B).
my_last4(A,B):-last(A,B).
my_tail5([_|TL],TL).
my_head6([H|_],H).
my_min_list7(A,B):-min_list(A,B).
my_min_list8(A,B):-min_list(A,B).
my_reverse9(A,B):-reverse(A,B).
my_head10([H|_],H).
my_sumlist11(A,B):-sumlist(A,B).
my_last12(A,B):-last(A,B).
my_min_list13(A,B):-min_list(A,B).
prim(my_max_list0/2).
prim(my_tail1/2).
prim(my_sumlist2/2).
prim(my_reverse3/2).
prim(my_last4/2).
prim(my_tail5/2).
prim(my_head6/2).
prim(my_min_list7/2).
prim(my_min_list8/2).
prim(my_reverse9/2).
prim(my_head10/2).
prim(my_sumlist11/2).
prim(my_last12/2).
prim(my_min_list13/2).
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
p([['c','p','w'],['e','x','t','r'],['h','n','o'],['j','f','y','m']],[['c','p'],['e','x','t'],['h','n'],['j','f','y']]).
p([['m','b','y'],['n','e','p','k'],['a','c','s','s'],['x','j','j','f']],[['m','b'],['n','e','p'],['a','c','s'],['x','j','j']]).
