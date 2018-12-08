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
my_last3(A,B):-last(A,B).
my_flatten4(A,B):-flatten(A,B).
my_msort5(A,B):-msort(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_min_list7(A,B):-min_list(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_even9(A):-0 is A mod 2.
my_lowercase10(A):-downcase_atom(A,A).
my_pred11(A,B):-succ(B,A),A > 0.
my_double12(N,M):-M is 2*N,M =< 10.
my_reverse13(A,B):-reverse(A,B).
my_tolower14(A,B):-downcase_atom(A,B).
my_odd15(A):-1 is A mod 2.
my_set16(A):-list_to_set(A,A).
my_toupper17(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_msort5,[list(int),list(int)]).
prim(my_succ6,[int,int]).
prim(my_min_list7,[list(int),int]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_even9,[int]).
prim(my_lowercase10,[char]).
prim(my_pred11,[int,int]).
prim(my_double12,[int,int]).
prim(my_reverse13,[list(T),list(T)]).
prim(my_tolower14,[char,char]).
prim(my_odd15,[int]).
prim(my_set16,[list(_)]).
prim(my_toupper17,[char,char]).
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
p(['E','R','C','V','f','V','G'],'V').
p(['m','k','z','W','W','T','T','d','a','Y'],'T').
p(['Z','r','C','y','r','o','b','q'],'r').
p(['G','u','W','Z','X','L','f','K','u'],'u').
p(['r','M','L','z','z','M','k','j','G'],'z').
q(['g','W','c','A','T','s','i','P','R','i','P'],'s').
q(['M','w','d','C','L','L','b'],'C').
q(['l','y','w','c','K','l','L'],'y').
q(['f','Z','L','L','L','l'],'Z').
q(['C','L','T','Q','D','Q','J','o'],'o').
