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
my_min_list3(A,B):-min_list(A,B).
my_set4(A):-list_to_set(A,A).
my_list_to_set5(A,B):-list_to_set(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_odd7(A):-1 is A mod 2.
my_flatten8(A,B):-flatten(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_tolower10(A,B):-downcase_atom(A,B).
my_msort11(A,B):-msort(A,B).
my_toupper12(A,B):-upcase_atom(A,B).
my_reverse13(A,B):-reverse(A,B).
my_succ14(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_min_list3,[list(int),int]).
prim(my_set4,[list(_)]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_uppercase6,[char]).
prim(my_odd7,[int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_double9,[int,int]).
prim(my_tolower10,[char,char]).
prim(my_msort11,[list(int),list(int)]).
prim(my_toupper12,[char,char]).
prim(my_reverse13,[list(T),list(T)]).
prim(my_succ14,[int,int]).
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
p(['C','W','W','E','R','m','W','A','v','m'],'W').
p(['r','w','f','v','n','A','w','j'],'w').
p(['O','S','M','c','H','D','c','G'],'c').
p(['K','j','K','r','s','R','v'],'K').
p(['x','T','m','r','G','O','m','v','d'],'m').
q(['K','g','k','v','m','D','Z','R','D','v','E'],'K').
q(['j','q','q','A','A','P','x','l','y','P','F'],'j').
q(['r','z','z','q','C','w','H','B','W','c'],'q').
q(['W','k','l','t','r','M','W','X','D'],'k').
q(['h','e','D','y','g','c','N','c'],'D').
