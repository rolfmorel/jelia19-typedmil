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
my_odd4(A):-1 is A mod 2.
my_set5(A):-list_to_set(A,A).
my_uppercase6(A):-upcase_atom(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_msort8(A,B):-msort(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_min_list3,[list(int),int]).
prim(my_odd4,[int]).
prim(my_set5,[list(_)]).
prim(my_uppercase6,[char]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_msort8,[list(int),list(int)]).
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
p(['R','R','u','n','f','D'],'R').
p(['L','i','e','E','c','e'],'e').
p(['t','M','B','H','Z','B','W'],'B').
p(['w','w','P','m','X','v','Z','L'],'w').
p(['g','j','E','g','M','T'],'g').
q(['k','z','Y','f','l','L','I','g','k','T','L'],'Y').
q(['T','p','n','c','n','l'],'c').
q(['z','z','r','A','A','I','B','H','z'],'B').
q(['A','h','T','N','u','h'],'N').
q(['B','F','e','N','B','T','l','a','Y','u','e'],'T').
