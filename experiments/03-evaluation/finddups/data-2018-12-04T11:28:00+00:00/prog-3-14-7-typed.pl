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
my_pred3(A,B):-succ(B,A),A > 0.
my_double4(N,M):-M is 2*N,M =< 10.
my_odd5(A):-1 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_msort7(A,B):-msort(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_succ9(A,B):-succ(A,B),B =< 10.
my_list_to_set10(A,B):-list_to_set(A,B).
my_even11(A):-0 is A mod 2.
my_sumlist12(A,B):-sumlist(A,B).
my_min_list13(A,B):-min_list(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_last15(A,B):-last(A,B).
my_reverse16(A,B):-reverse(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_double4,[int,int]).
prim(my_odd5,[int]).
prim(my_set6,[list(_)]).
prim(my_msort7,[list(int),list(int)]).
prim(my_uppercase8,[char]).
prim(my_succ9,[int,int]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_even11,[int]).
prim(my_sumlist12,[list(int),int]).
prim(my_min_list13,[list(int),int]).
prim(my_lowercase14,[char]).
prim(my_last15,[list(T),T]).
prim(my_reverse16,[list(T),list(T)]).
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
p(['t','q','v','N','q'],'q').
p(['T','E','s','K','Y','V','d','K','f'],'K').
p(['C','a','w','O','g','a','t','f'],'a').
p(['D','P','n','L','J','P'],'P').
p(['p','B','o','p','i','p'],'p').
q(['T','v','A','v','t','j','a','H','o','B','m'],'A').
q(['x','u','u','H','q','S','P','r'],'q').
q(['l','b','K','p','K','E','Q','X','r'],'E').
q(['O','B','l','i','i','A','z'],'O').
q(['S','Z','P','l','k','T','l','r'],'Z').
