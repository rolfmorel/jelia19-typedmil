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
my_len3(A,B):-length(A,B).
my_last4(A,B):-last(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_even7(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_last4,[list(T),T]).
prim(my_toupper5,[char,char]).
prim(my_sumlist6,[list(int),int]).
prim(my_even7,[int]).
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
p(['f','d','c','t','N','F','S','c'],'c').
p(['C','F','w','a','X','X'],'X').
p(['F','N','G','h','S','F'],'F').
p(['R','N','D','I','R','u'],'R').
p(['f','g','y','Q','q','Q'],'Q').
q(['c','a','r','W','R','z','A','d','F','d','o'],'A').
q(['I','N','z','b','o','z'],'o').
q(['U','i','y','h','H','Y','i','k'],'y').
q(['b','X','y','y','z','M','n','c'],'X').
q(['n','v','T','p','N','E','n','r','z'],'T').
