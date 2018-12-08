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
my_double3(N,M):-M is 2*N,M =< 10.
my_msort4(A,B):-msort(A,B).
my_max_list5(A,B):-max_list(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_odd7(A):-1 is A mod 2.
my_sumlist8(A,B):-sumlist(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_lowercase10(A):-downcase_atom(A,A).
my_toupper11(A,B):-upcase_atom(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_set13(A):-list_to_set(A,A).
my_last14(A,B):-last(A,B).
my_even15(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_double3,[int,int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_max_list5,[list(int),int]).
prim(my_pred6,[int,int]).
prim(my_odd7,[int]).
prim(my_sumlist8,[list(int),int]).
prim(my_succ9,[int,int]).
prim(my_lowercase10,[char]).
prim(my_toupper11,[char,char]).
prim(my_list_to_set12,[list(T),list(T)]).
prim(my_set13,[list(_)]).
prim(my_last14,[list(T),T]).
prim(my_even15,[int]).
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
p(['d','d','d','y','w','K','M'],'d').
p(['l','L','S','l','u'],'l').
p(['X','p','Z','X','Z','O','L','W','Z','v'],'X').
p(['E','f','P','G','E','h'],'E').
p(['K','K','i','J','Q'],'K').
q(['b','v','u','p','j','Q','G','E','G','p','q'],'Q').
q(['M','c','k','G','H','n','H','Y','{','n','i'],'{').
q(['K','c','a','c','h','C','z','v','k','D','z'],'h').
q(['M','b','M','b','k','c','K','N','x'],'N').
q(['l','l','G','m','f','t','B'],'f').
