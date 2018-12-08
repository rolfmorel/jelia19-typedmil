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
my_odd4(A):-1 is A mod 2.
my_set5(A):-list_to_set(A,A).
my_uppercase6(A):-upcase_atom(A,A).
my_pred7(A,B):-succ(B,A),A > 0.
my_lowercase8(A):-downcase_atom(A,A).
my_tolower9(A,B):-downcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_odd4,[int]).
prim(my_set5,[list(_)]).
prim(my_uppercase6,[char]).
prim(my_pred7,[int,int]).
prim(my_lowercase8,[char]).
prim(my_tolower9,[char,char]).
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
