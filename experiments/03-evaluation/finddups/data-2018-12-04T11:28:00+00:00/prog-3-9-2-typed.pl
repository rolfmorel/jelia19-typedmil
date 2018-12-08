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
my_flatten4(A,B):-flatten(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_min_list6(A,B):-min_list(A,B).
my_set7(A):-list_to_set(A,A).
my_toupper8(A,B):-upcase_atom(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_sumlist10(A,B):-sumlist(A,B).
my_len11(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_uppercase3,[char]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_succ5,[int,int]).
prim(my_min_list6,[list(int),int]).
prim(my_set7,[list(_)]).
prim(my_toupper8,[char,char]).
prim(my_double9,[int,int]).
prim(my_sumlist10,[list(int),int]).
prim(my_len11,[list(_),int]).
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
p(['P','u','P','i','x'],'P').
p(['A','u','D','A','M','T','x','C'],'A').
p(['z','F','V','Y','h','i','C','C','c','m'],'C').
p(['Y','B','P','M','T','B'],'B').
p(['d','x','l','s','A','Q','k','L','k'],'k').
q(['G','u','u','C','u','j','t','Z'],'t').
q(['x','e','x','[','t','S','Q','h','n'],'[').
q(['F','y','b','y','F','Z'],'Z').
q(['X','u','G','P','u','V','x','m','Q','b','n'],'n').
q(['x','c','I','f','r','M','M'],'c').
