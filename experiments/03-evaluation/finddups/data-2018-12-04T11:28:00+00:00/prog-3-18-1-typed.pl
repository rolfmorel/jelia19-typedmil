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
my_last4(A,B):-last(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_uppercase7(A):-upcase_atom(A,A).
my_even8(A):-0 is A mod 2.
my_tolower9(A,B):-downcase_atom(A,B).
my_min_list10(A,B):-min_list(A,B).
my_flatten11(A,B):-flatten(A,B).
my_set12(A):-list_to_set(A,A).
my_succ13(A,B):-succ(A,B),B =< 10.
my_double14(N,M):-M is 2*N,M =< 10.
my_len15(A,B):-length(A,B).
my_max_list16(A,B):-max_list(A,B).
my_pred17(A,B):-succ(B,A),A > 0.
my_list_to_set18(A,B):-list_to_set(A,B).
my_reverse19(A,B):-reverse(A,B).
my_odd20(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_toupper3,[char,char]).
prim(my_last4,[list(T),T]).
prim(my_sumlist5,[list(int),int]).
prim(my_lowercase6,[char]).
prim(my_uppercase7,[char]).
prim(my_even8,[int]).
prim(my_tolower9,[char,char]).
prim(my_min_list10,[list(int),int]).
prim(my_flatten11,[list(list(T)),list(T)]).
prim(my_set12,[list(_)]).
prim(my_succ13,[int,int]).
prim(my_double14,[int,int]).
prim(my_len15,[list(_),int]).
prim(my_max_list16,[list(int),int]).
prim(my_pred17,[int,int]).
prim(my_list_to_set18,[list(T),list(T)]).
prim(my_reverse19,[list(T),list(T)]).
prim(my_odd20,[int]).
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
p(['L','n','M','q','L','L'],'L').
p(['q','P','K','E','p','A','E','e','d'],'E').
p(['f','T','T','o','z','o','R'],'T').
p(['S','l','S','i','d','u','H','x'],'S').
p(['P','v','l','D','D','F','B','d','i','j'],'D').
q(['h','y','t','s','a','u','A','H','h','U','y'],'H').
q(['T','x','Z','a','M','a','r','C'],'M').
q(['R','M','L','T','L','m','k'],'k').
q(['J','B','g','N','G','A','m','R','A'],'N').
q(['E','h','F','Q','j','h','h'],'Q').
