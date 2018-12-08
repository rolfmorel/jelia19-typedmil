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
my_set3(A):-list_to_set(A,A).
my_last4(A,B):-last(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_succ7(A,B):-succ(A,B),B =< 10.
my_list_to_set8(A,B):-list_to_set(A,B).
my_double9(N,M):-M is 2*N,M =< 10.
my_uppercase10(A):-upcase_atom(A,A).
my_max_list11(A,B):-max_list(A,B).
my_msort12(A,B):-msort(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_set3,[list(_)]).
prim(my_last4,[list(T),T]).
prim(my_tolower5,[char,char]).
prim(my_lowercase6,[char]).
prim(my_succ7,[int,int]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_double9,[int,int]).
prim(my_uppercase10,[char]).
prim(my_max_list11,[list(int),int]).
prim(my_msort12,[list(int),list(int)]).
prim(my_toupper13,[char,char]).
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
p(['A','C','i','w','w','R'],'w').
p(['K','J','w','w','h','N'],'w').
p(['y','S','R','c','T','S','g'],'S').
p(['q','j','A','U','C','o','C'],'C').
p(['V','U','J','b','T','U','D','e','k','H'],'U').
q(['x','L','y','t','T','T'],'y').
q(['b','I','J','n','H','B','A','J'],'H').
q(['z','x','s','R','n','y','y'],'R').
q(['A','s','B','e','W','B','R'],'A').
q(['p','b','u','a','p','M','B','P','U','v'],'a').
