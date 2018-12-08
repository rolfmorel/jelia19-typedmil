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
my_sumlist3(A,B):-sumlist(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_odd5(A):-1 is A mod 2.
my_succ6(A,B):-succ(A,B),B =< 10.
my_pred7(A,B):-succ(B,A),A > 0.
my_max_list8(A,B):-max_list(A,B).
my_uppercase9(A):-upcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_odd5,[int]).
prim(my_succ6,[int,int]).
prim(my_pred7,[int,int]).
prim(my_max_list8,[list(int),int]).
prim(my_uppercase9,[char]).
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
p(['j','T','W','W','q','v','y','k'],'W').
p(['O','v','n','v','J'],'v').
p(['A','W','g','c','W','m','O'],'W').
p(['L','Q','g','y','M','E','y','q'],'y').
p(['p','Q','h','y','o','F','Q','y'],'Q').
q(['N','Y','g','q','I','M','o','n','u','N'],'M').
q(['b','F','f','M','Z','q','M'],'F').
q(['F','K','F','y','P','a','A'],'P').
q(['T','s','r','T','i','V','r'],'i').
q(['b','n','O','u','G','b','H','x'],'u').
