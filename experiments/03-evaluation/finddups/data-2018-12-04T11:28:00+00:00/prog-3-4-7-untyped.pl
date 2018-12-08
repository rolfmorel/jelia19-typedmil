:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
%metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_element2(A,B):-member(B,A).
my_odd3(A):-1 is A mod 2.
my_max_list4(A,B):-max_list(A,B).
my_even5(A):-0 is A mod 2.
my_pred6(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_odd3/1).
prim(my_max_list4/2).
prim(my_even5/1).
prim(my_pred6/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p(['r','P','X','r','Q','r','Y','p','E','T'],'r').
p(['N','b','F','a','N','x','i','D'],'N').
p(['G','q','W','w','W'],'W').
p(['M','w','M','u','o','f'],'M').
p(['d','i','L','i','u'],'i').
q(['B','H','x','u','c','g','n','x','u'],'B').
q(['H','J','E','r','j','X','j','X','t','i'],'r').
q(['N','y','U','J','u','u','b','O','k','Z','C'],'N').
q(['R','X','K','x','B','A','K'],'A').
q(['x','z','B','i','D','e','x'],'e').
