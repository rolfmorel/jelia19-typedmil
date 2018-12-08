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
my_succ3(A,B):-succ(A,B),B =< 10.
my_flatten4(A,B):-flatten(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_msort8(A,B):-msort(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_pred10(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_tolower5,[char,char]).
prim(my_sumlist6,[list(int),int]).
prim(my_double7,[int,int]).
prim(my_msort8,[list(int),list(int)]).
prim(my_lowercase9,[char]).
prim(my_pred10,[int,int]).
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
p(['H','b','u','J','b','f','h','b','b'],'b').
p(['M','u','G','g','T','M','j'],'M').
p(['Q','Z','T','x','Z','Y'],'Z').
p(['o','b','Q','b','s','N','b','j','n','a'],'b').
p(['g','U','C','V','S','C','R','h','l','E'],'C').
q(['C','J','J','J','K','l','g'],'g').
q(['p','D','N','s','N','i'],'p').
q(['H','B','h','L','p','M','E','B'],'L').
q(['n','c','H','f','B','M','f','Y','h'],'M').
q(['o','q','V','e','e','T','R','p'],'T').
