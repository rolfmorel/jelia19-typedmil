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
my_list_to_set3(A,B):-list_to_set(A,B).
my_sumlist4(A,B):-sumlist(A,B).
my_msort5(A,B):-msort(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
my_max_list7(A,B):-max_list(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_last9(A,B):-last(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_list_to_set3/2).
prim(my_sumlist4/2).
prim(my_msort5/2).
prim(my_tolower6/2).
prim(my_max_list7/2).
prim(my_double8/2).
prim(my_last9/2).
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
p(['h','V','X','b','r','X','s','e','x','m'],'X').
p(['r','x','r','b','U','H','L'],'r').
p(['r','R','P','p','W','p'],'p').
p(['X','T','W','G','I','S','S','P','O','p'],'S').
p(['e','y','y','h','X','q'],'y').
q(['d','L','c','Z','T','c'],'L').
q(['R','Y','A','O','l','l'],'O').
q(['N','O','G','v','y','s','T','U','d','d'],'T').
q(['Z','a','d','l','T','w','N','J','T'],'J').
q(['C','i','s','b','v','w','j','j'],'w').
