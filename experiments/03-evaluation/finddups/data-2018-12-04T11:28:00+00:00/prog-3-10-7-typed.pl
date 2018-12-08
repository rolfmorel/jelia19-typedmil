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
my_flatten3(A,B):-flatten(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_min_list5(A,B):-min_list(A,B).
my_odd6(A):-1 is A mod 2.
my_reverse7(A,B):-reverse(A,B).
my_lowercase8(A):-downcase_atom(A,A).
my_tolower9(A,B):-downcase_atom(A,B).
my_uppercase10(A):-upcase_atom(A,A).
my_len11(A,B):-length(A,B).
my_set12(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_min_list5,[list(int),int]).
prim(my_odd6,[int]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_lowercase8,[char]).
prim(my_tolower9,[char,char]).
prim(my_uppercase10,[char]).
prim(my_len11,[list(_),int]).
prim(my_set12,[list(_)]).
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
p(['L','m','b','V','A','n','b','S','r','z'],'b').
p(['X','K','s','B','K','h','n'],'K').
p(['s','v','N','N','L','c','c','L','S','F'],'N').
p(['y','m','n','y','M','b','s','U','M'],'y').
p(['d','k','m','Q','m'],'m').
q(['X','q','c','P','H','K','Z','D','q'],'X').
q(['T','E','S','b','w','A','B','A','Q','A'],'b').
q(['m','z','m','U','M','Q'],'z').
q(['x','E','C','E','J','K','o','I','M','o','g'],'J').
q(['P','g','W','w','c','c'],'w').
