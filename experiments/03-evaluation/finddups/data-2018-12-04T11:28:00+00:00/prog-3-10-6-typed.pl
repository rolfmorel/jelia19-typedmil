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
my_double3(N,M):-M is 2*N,M =< 10.
my_tolower4(A,B):-downcase_atom(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_max_list6(A,B):-max_list(A,B).
my_odd7(A):-1 is A mod 2.
my_last8(A,B):-last(A,B).
my_msort9(A,B):-msort(A,B).
my_uppercase10(A):-upcase_atom(A,A).
my_succ11(A,B):-succ(A,B),B =< 10.
my_even12(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_double3,[int,int]).
prim(my_tolower4,[char,char]).
prim(my_lowercase5,[char]).
prim(my_max_list6,[list(int),int]).
prim(my_odd7,[int]).
prim(my_last8,[list(T),T]).
prim(my_msort9,[list(int),list(int)]).
prim(my_uppercase10,[char]).
prim(my_succ11,[int,int]).
prim(my_even12,[int]).
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
p(['R','K','E','j','E','V','B'],'E').
p(['E','k','p','x','T','p','x'],'p').
p(['d','N','N','t','f','e','l','D','e','y'],'e').
p(['a','a','E','U','j'],'a').
p(['r','N','y','X','Z','X','T'],'X').
q(['R','x','g','B','d','g','Y'],'x').
q(['s','D','s','K','B','c'],'c').
q(['H','q','m','O','O','i'],'q').
q(['U','U','x','[','r','B','i','B'],'[').
q(['n','z','t','n','p','D','t','z','T','m','F'],'F').
