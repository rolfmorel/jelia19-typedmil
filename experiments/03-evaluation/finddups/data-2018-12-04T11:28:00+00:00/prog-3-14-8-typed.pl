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
my_toupper3(A,B):-upcase_atom(A,B).
my_flatten4(A,B):-flatten(A,B).
my_msort5(A,B):-msort(A,B).
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_last9(A,B):-last(A,B).
my_pred10(A,B):-succ(B,A),A > 0.
my_lowercase11(A):-downcase_atom(A,A).
my_succ12(A,B):-succ(A,B),B =< 10.
my_list_to_set13(A,B):-list_to_set(A,B).
my_double14(N,M):-M is 2*N,M =< 10.
my_even15(A):-0 is A mod 2.
my_sumlist16(A,B):-sumlist(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_toupper3,[char,char]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_msort5,[list(int),list(int)]).
prim(my_len6,[list(_),int]).
prim(my_max_list7,[list(int),int]).
prim(my_tolower8,[char,char]).
prim(my_last9,[list(T),T]).
prim(my_pred10,[int,int]).
prim(my_lowercase11,[char]).
prim(my_succ12,[int,int]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_double14,[int,int]).
prim(my_even15,[int]).
prim(my_sumlist16,[list(int),int]).
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
p(['F','Y','p','M','q','J','M'],'M').
p(['v','c','I','c','b','G'],'c').
p(['X','X','B','S','m','T','v','X'],'X').
p(['a','e','e','q','Y','z','e','q','A','K'],'e').
p(['B','w','M','a','a','T','r','j','m'],'a').
q(['X','z','A','j','A','e','v','S'],'S').
q(['Y','w','W','H','K','m','m'],'w').
q(['S','V','w','r','S','x'],'r').
q(['k','e','a','s','a','t','n'],'t').
q(['D','F','x','G','i','e','b','s','u','e'],'s').
