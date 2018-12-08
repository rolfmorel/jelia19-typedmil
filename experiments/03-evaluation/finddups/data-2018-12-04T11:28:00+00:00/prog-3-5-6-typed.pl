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
my_toupper3(A,B):-upcase_atom(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_sumlist5(A,B):-sumlist(A,B).
my_even6(A):-0 is A mod 2.
my_len7(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_toupper3,[char,char]).
prim(my_lowercase4,[char]).
prim(my_sumlist5,[list(int),int]).
prim(my_even6,[int]).
prim(my_len7,[list(_),int]).
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
p(['x','I','X','U','b','E','X','j','E'],'E').
p(['d','Z','Q','b','f','C','y','F','y','O'],'y').
p(['E','w','J','H','H','Q','M'],'H').
p(['H','S','R','g','G','g','D','E','s'],'g').
p(['j','p','T','z','c','p'],'p').
q(['q','A','U','F','A','t','O'],'F').
q(['V','C','T','i','F','i','K','C'],'K').
q(['R','o','Y','q','v','G','m','i','M','p','q'],'v').
q(['t','j','r','O','h','k','t','O','H','u','l'],'j').
q(['y','o','e','x','v','C','X','f','j','x'],'o').
