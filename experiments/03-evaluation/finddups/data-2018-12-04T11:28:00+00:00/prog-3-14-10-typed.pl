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
my_even4(A):-0 is A mod 2.
my_lowercase5(A):-downcase_atom(A,A).
my_toupper6(A,B):-upcase_atom(A,B).
my_set7(A):-list_to_set(A,A).
my_len8(A,B):-length(A,B).
my_odd9(A):-1 is A mod 2.
my_pred10(A,B):-succ(B,A),A > 0.
my_double11(N,M):-M is 2*N,M =< 10.
my_sumlist12(A,B):-sumlist(A,B).
my_msort13(A,B):-msort(A,B).
my_tolower14(A,B):-downcase_atom(A,B).
my_min_list15(A,B):-min_list(A,B).
my_succ16(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_even4,[int]).
prim(my_lowercase5,[char]).
prim(my_toupper6,[char,char]).
prim(my_set7,[list(_)]).
prim(my_len8,[list(_),int]).
prim(my_odd9,[int]).
prim(my_pred10,[int,int]).
prim(my_double11,[int,int]).
prim(my_sumlist12,[list(int),int]).
prim(my_msort13,[list(int),list(int)]).
prim(my_tolower14,[char,char]).
prim(my_min_list15,[list(int),int]).
prim(my_succ16,[int,int]).
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
p(['x','c','s','v','Y','s','Q'],'s').
p(['y','g','y','t','j','a','A'],'y').
p(['o','F','j','X','v','c','X'],'X').
p(['P','L','L','B','p','p','s','K'],'p').
p(['X','F','k','l','D','Q','k'],'k').
q(['C','c','W','U','T','M','M','M','M','z'],'T').
q(['H','H','j','r','J','k'],'r').
q(['D','j','P','g','T','C','o','j','v','R'],'o').
q(['j','D','D','V','x','d','T','I','H','A'],'I').
q(['o','K','R','I','R','P','d'],'P').
