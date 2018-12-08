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
my_msort3(A,B):-msort(A,B).
my_double4(N,M):-M is 2*N,M =< 10.
my_toupper5(A,B):-upcase_atom(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_set7(A):-list_to_set(A,A).
my_last8(A,B):-last(A,B).
my_tolower9(A,B):-downcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_msort3,[list(int),list(int)]).
prim(my_double4,[int,int]).
prim(my_toupper5,[char,char]).
prim(my_uppercase6,[char]).
prim(my_set7,[list(_)]).
prim(my_last8,[list(T),T]).
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
p(['D','u','i','V','H','H','Z'],'H').
p(['D','X','A','w','G','D','f'],'D').
p(['P','j','J','L','L','C'],'L').
p(['f','O','a','t','d','u','p','R','c','u'],'u').
p(['q','C','F','j','G','e','j','a'],'j').
q(['p','j','C','K','e','F','f','K'],'j').
q(['v','Z','v','Z','e','n'],'n').
q(['e','l','P','j','O','A','i','L','U','U','U'],'l').
q(['{','V','y','d','y','M','X','D','d','c','r'],'{').
q(['w','t','t','E','t','T','K','l','Z','N'],'Z').
