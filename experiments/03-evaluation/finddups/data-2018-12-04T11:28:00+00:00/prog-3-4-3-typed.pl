:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
%metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_element2(A,B):-member(B,A).
my_uppercase3(A):-upcase_atom(A,A).
my_succ4(A,B):-succ(A,B),B =< 10.
my_set5(A):-list_to_set(A,A).
my_toupper6(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_uppercase3,[char]).
prim(my_succ4,[int,int]).
prim(my_set5,[list(_)]).
prim(my_toupper6,[char,char]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),char],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p(['K','L','h','C','r','K','h','P','M'],'K').
p(['W','p','u','p','N','t','t','w','z','a'],'p').
p(['x','z','o','i','e','N','X','e','Z'],'e').
p(['g','g','E','O','E','J','b','e'],'g').
p(['h','D','d','i','a','x','u','h'],'h').
q(['s','J','z','B','L','Q','R','N','d','L','N'],'B').
q(['Z','c','W','Z','F','g'],'g').
q(['C','k','x','H','i','C','T','W','A','a','a'],'k').
q(['Q','R','f','E','g','d','s','f','W','T','J'],'T').
q(['S','F','F','m','g','C','T'],'m').
