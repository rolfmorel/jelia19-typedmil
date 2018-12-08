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
my_even3(A):-0 is A mod 2.
my_max_list4(A,B):-max_list(A,B).
my_msort5(A,B):-msort(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_tolower7(A,B):-downcase_atom(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_sumlist9(A,B):-sumlist(A,B).
my_last10(A,B):-last(A,B).
my_reverse11(A,B):-reverse(A,B).
my_min_list12(A,B):-min_list(A,B).
my_odd13(A):-1 is A mod 2.
my_list_to_set14(A,B):-list_to_set(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_even3,[int]).
prim(my_max_list4,[list(int),int]).
prim(my_msort5,[list(int),list(int)]).
prim(my_pred6,[int,int]).
prim(my_tolower7,[char,char]).
prim(my_succ8,[int,int]).
prim(my_sumlist9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_reverse11,[list(T),list(T)]).
prim(my_min_list12,[list(int),int]).
prim(my_odd13,[int]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_double15,[int,int]).
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
p(['H','H','Z','G','g'],'H').
p(['u','b','U','b','r','S','k'],'b').
p(['n','b','F','j','z','w','n'],'n').
p(['D','D','c','l','R','x','F'],'D').
p(['W','S','k','W','N'],'W').
q(['R','e','W','B','B','X','t','A','h','Q','Z'],'t').
q(['l','X','G','X','C','U','E'],'U').
q(['r','Q','Q','Z','C','S','I','a'],'C').
q(['e','W','X','P','y','d','A','S','O','P'],'X').
q(['H','p','f','A','h','E','m','O','O','y'],'m').
