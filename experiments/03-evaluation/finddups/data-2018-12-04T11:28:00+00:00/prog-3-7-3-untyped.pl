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
my_succ3(A,B):-succ(A,B),B =< 10.
my_odd4(A):-1 is A mod 2.
my_set5(A):-list_to_set(A,A).
my_uppercase6(A):-upcase_atom(A,A).
my_pred7(A,B):-succ(B,A),A > 0.
my_lowercase8(A):-downcase_atom(A,A).
my_tolower9(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_succ3/2).
prim(my_odd4/1).
prim(my_set5/1).
prim(my_uppercase6/1).
prim(my_pred7/2).
prim(my_lowercase8/1).
prim(my_tolower9/2).
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
p(['u','u','p','P','y','K','Q','K','m','t'],'K').
p(['v','n','A','U','I','A','x'],'A').
p(['G','F','R','R','Y','q','O'],'R').
p(['O','u','K','Y','r','u'],'u').
p(['x','C','M','e','m','r','C'],'C').
q(['m','V','S','l','E','P','W','E','z','a','R'],'V').
q(['a','H','g','F','c','F'],'a').
q(['C','c','f','f','j','I','p','q','m','c'],'p').
q(['z','H','R','z','h','q','U','L','V','V','h'],'U').
q(['P','T','E','Q','W','o','t','T','[','a'],'[').
