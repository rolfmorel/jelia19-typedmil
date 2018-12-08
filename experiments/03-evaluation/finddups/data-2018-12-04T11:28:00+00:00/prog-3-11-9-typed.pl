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
my_set5(A):-list_to_set(A,A).
my_lowercase6(A):-downcase_atom(A,A).
my_double7(N,M):-M is 2*N,M =< 10.
my_uppercase8(A):-upcase_atom(A,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_even10(A):-0 is A mod 2.
my_flatten11(A,B):-flatten(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_sumlist13(A,B):-sumlist(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_list_to_set3,[list(T),list(T)]).
prim(my_tolower4,[char,char]).
prim(my_set5,[list(_)]).
prim(my_lowercase6,[char]).
prim(my_double7,[int,int]).
prim(my_uppercase8,[char]).
prim(my_pred9,[int,int]).
prim(my_even10,[int]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_succ12,[int,int]).
prim(my_sumlist13,[list(int),int]).
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
p(['C','D','A','D','c'],'D').
p(['g','g','A','g','m','m'],'g').
p(['t','h','d','c','N','r','d','x'],'d').
p(['D','N','e','D','E','P','w','O','V'],'D').
p(['d','m','X','l','e','v','m','Z'],'m').
q(['g','x','R','x','G','T','w','p'],'w').
q(['m','D','o','h','o','F'],'h').
q(['M','H','t','W','M','t','t','w'],'H').
q(['u','I','A','h','c','A'],'c').
q(['y','T','T','S','T','t','d','g'],'d').
