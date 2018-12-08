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
my_double3(N,M):-M is 2*N,M =< 10.
my_lowercase4(A):-downcase_atom(A,A).
my_sumlist5(A,B):-sumlist(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_double3/2).
prim(my_lowercase4/1).
prim(my_sumlist5/2).
prim(my_tolower6/2).
prim(my_reverse7/2).
prim(my_last8/2).
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
p(['z','b','M','i','M','z','p','p','C','H'],'M').
p(['K','O','c','Q','V','x','c','Z'],'c').
p(['x','M','m','x','x','D'],'x').
p(['i','N','K','l','t','i','N','N','W'],'N').
p(['x','F','I','z','N','R','I'],'I').
q(['W','L','Y','I','U','j','W','c','P'],'L').
q(['P','P','q','w','z','F','E','s','H','b','H'],'s').
q(['d','T','L','l','Y','T'],'Y').
q(['s','F','j','f','S','I','T','T','L','H','u'],'I').
q(['E','v','D','l','r','E','W','K','B'],'K').
