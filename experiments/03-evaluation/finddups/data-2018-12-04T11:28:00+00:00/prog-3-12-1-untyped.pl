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
my_min_list3(A,B):-min_list(A,B).
my_set4(A):-list_to_set(A,A).
my_list_to_set5(A,B):-list_to_set(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_odd7(A):-1 is A mod 2.
my_flatten8(A,B):-flatten(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_tolower10(A,B):-downcase_atom(A,B).
my_msort11(A,B):-msort(A,B).
my_toupper12(A,B):-upcase_atom(A,B).
my_reverse13(A,B):-reverse(A,B).
my_succ14(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_set4/1).
prim(my_list_to_set5/2).
prim(my_uppercase6/1).
prim(my_odd7/1).
prim(my_flatten8/2).
prim(my_double9/2).
prim(my_tolower10/2).
prim(my_msort11/2).
prim(my_toupper12/2).
prim(my_reverse13/2).
prim(my_succ14/2).
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
p(['C','W','W','E','R','m','W','A','v','m'],'W').
p(['r','w','f','v','n','A','w','j'],'w').
p(['O','S','M','c','H','D','c','G'],'c').
p(['K','j','K','r','s','R','v'],'K').
p(['x','T','m','r','G','O','m','v','d'],'m').
q(['K','g','k','v','m','D','Z','R','D','v','E'],'K').
q(['j','q','q','A','A','P','x','l','y','P','F'],'j').
q(['r','z','z','q','C','w','H','B','W','c'],'q').
q(['W','k','l','t','r','M','W','X','D'],'k').
q(['h','e','D','y','g','c','N','c'],'D').
