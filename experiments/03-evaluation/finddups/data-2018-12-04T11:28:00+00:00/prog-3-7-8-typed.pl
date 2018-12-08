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
my_sumlist4(A,B):-sumlist(A,B).
my_msort5(A,B):-msort(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
my_max_list7(A,B):-max_list(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_last9(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_sumlist4,[list(int),int]).
prim(my_msort5,[list(int),list(int)]).
prim(my_tolower6,[char,char]).
prim(my_max_list7,[list(int),int]).
prim(my_double8,[int,int]).
prim(my_last9,[list(T),T]).
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
p(['h','V','X','b','r','X','s','e','x','m'],'X').
p(['r','x','r','b','U','H','L'],'r').
p(['r','R','P','p','W','p'],'p').
p(['X','T','W','G','I','S','S','P','O','p'],'S').
p(['e','y','y','h','X','q'],'y').
q(['d','L','c','Z','T','c'],'L').
q(['R','Y','A','O','l','l'],'O').
q(['N','O','G','v','y','s','T','U','d','d'],'T').
q(['Z','a','d','l','T','w','N','J','T'],'J').
q(['C','i','s','b','v','w','j','j'],'w').
