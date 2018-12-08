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
my_lowercase3(A):-downcase_atom(A,A).
my_min_list4(A,B):-min_list(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_odd9(A):-1 is A mod 2.
my_succ10(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_lowercase3,[char]).
prim(my_min_list4,[list(int),int]).
prim(my_tolower5,[char,char]).
prim(my_len6,[list(_),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_uppercase8,[char]).
prim(my_odd9,[int]).
prim(my_succ10,[int,int]).
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
p(['g','R','O','X','R','i','R','o'],'R').
p(['f','W','M','O','n','n','Q'],'n').
p(['e','X','V','m','l','X','K','g'],'X').
p(['e','I','K','I','f','u','X','f','R'],'I').
p(['h','D','h','e','y','A','i','Q'],'h').
q(['n','E','E','Q','R','O','o'],'R').
q(['B','R','K','N','u','u','C','Y'],'B').
q(['p','U','V','f','U','Y','T','H','W'],'V').
q(['X','k','k','C','C','P'],'X').
q(['S','u','C','K','r','C','P'],'P').
