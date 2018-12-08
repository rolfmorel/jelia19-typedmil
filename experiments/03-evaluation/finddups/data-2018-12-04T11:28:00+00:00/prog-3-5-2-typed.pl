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
my_sumlist3(A,B):-sumlist(A,B).
my_even4(A):-0 is A mod 2.
my_uppercase5(A):-upcase_atom(A,A).
my_len6(A,B):-length(A,B).
my_last7(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_even4,[int]).
prim(my_uppercase5,[char]).
prim(my_len6,[list(_),int]).
prim(my_last7,[list(T),T]).
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
p(['P','P','d','Q','q','c','q'],'q').
p(['o','G','z','V','w','o','U','q'],'o').
p(['J','M','T','f','E','E','F'],'E').
p(['g','E','r','f','x','T','g','U','E'],'g').
p(['R','h','M','V','J','e','T','Y','B','B'],'B').
q(['q','K','c','q','M','Q','N','K'],'M').
q(['m','b','a','S','A','K','m','M'],'A').
q(['l','t','F','l','d','S'],'F').
q(['u','Y','y','W','q','F','f','Y','z'],'u').
q(['X','E','A','M','U','M','p','Z','t','F','D'],'X').
