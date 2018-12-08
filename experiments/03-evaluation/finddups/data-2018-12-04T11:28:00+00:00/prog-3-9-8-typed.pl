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
my_odd3(A):-1 is A mod 2.
my_double4(N,M):-M is 2*N,M =< 10.
my_len5(A,B):-length(A,B).
my_set6(A):-list_to_set(A,A).
my_last7(A,B):-last(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_toupper9(A,B):-upcase_atom(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_even11(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_odd3,[int]).
prim(my_double4,[int,int]).
prim(my_len5,[list(_),int]).
prim(my_set6,[list(_)]).
prim(my_last7,[list(T),T]).
prim(my_uppercase8,[char]).
prim(my_toupper9,[char,char]).
prim(my_lowercase10,[char]).
prim(my_even11,[int]).
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
p(['i','a','G','X','S','A','A','S'],'A').
p(['P','N','s','s','u','i'],'s').
p(['o','P','A','Z','Z','M','w','U','o'],'o').
p(['R','W','G','R','A','k'],'R').
p(['d','e','e','G','G'],'e').
q(['j','x','j','W','R','T','a','r','c','N','A'],'T').
q(['a','t','T','Y','r','i','z','t'],'a').
q(['H','Y','W','e','e','v','M','e','X','O'],'v').
q(['U','M','s','a','f','Q','N','g','N','w','N'],'U').
q(['N','B','p','Q','i','i'],'Q').
