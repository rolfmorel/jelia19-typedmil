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
my_list_to_set3(A,B):-list_to_set(A,B).
my_tolower4(A,B):-downcase_atom(A,B).
my_reverse5(A,B):-reverse(A,B).
my_min_list6(A,B):-min_list(A,B).
my_odd7(A):-1 is A mod 2.
my_lowercase8(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_tolower4,[char,char]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_min_list6,[list(int),int]).
prim(my_odd7,[int]).
prim(my_lowercase8,[char]).
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
p(['V','K','f','i','f','H','Y','p','J','f'],'f').
p(['i','N','i','o','s','Y','C'],'i').
p(['M','a','M','a','a','J','W','v','k','z'],'a').
p(['c','u','r','t','M','i','t','b','S'],'t').
p(['Y','e','L','Y','H','k'],'Y').
q(['X','H','P','l','X','a'],'l').
q(['T','O','Z','u','n','n','F'],'Z').
q(['V','A','s','A','o','M'],'M').
q(['v','t','q','t','t','d','T','U'],'U').
q(['f','s','M','v','R','B','k','d','k','G'],'B').
