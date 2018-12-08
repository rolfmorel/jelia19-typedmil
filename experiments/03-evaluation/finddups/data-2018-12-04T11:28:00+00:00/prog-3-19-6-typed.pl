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
my_pred4(A,B):-succ(B,A),A > 0.
my_flatten5(A,B):-flatten(A,B).
my_reverse6(A,B):-reverse(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_odd9(A):-1 is A mod 2.
my_max_list10(A,B):-max_list(A,B).
my_msort11(A,B):-msort(A,B).
my_uppercase12(A):-upcase_atom(A,A).
my_set13(A):-list_to_set(A,A).
my_toupper14(A,B):-upcase_atom(A,B).
my_len15(A,B):-length(A,B).
my_last16(A,B):-last(A,B).
my_min_list17(A,B):-min_list(A,B).
my_lowercase18(A):-downcase_atom(A,A).
my_even19(A):-0 is A mod 2.
my_sumlist20(A,B):-sumlist(A,B).
my_list_to_set21(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_double3,[int,int]).
prim(my_pred4,[int,int]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_tolower7,[char,char]).
prim(my_succ8,[int,int]).
prim(my_odd9,[int]).
prim(my_max_list10,[list(int),int]).
prim(my_msort11,[list(int),list(int)]).
prim(my_uppercase12,[char]).
prim(my_set13,[list(_)]).
prim(my_toupper14,[char,char]).
prim(my_len15,[list(_),int]).
prim(my_last16,[list(T),T]).
prim(my_min_list17,[list(int),int]).
prim(my_lowercase18,[char]).
prim(my_even19,[int]).
prim(my_sumlist20,[list(int),int]).
prim(my_list_to_set21,[list(T),list(T)]).
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
p(['p','m','j','B','B','t'],'B').
p(['w','g','m','f','V','Y','V','e'],'V').
p(['D','U','A','i','A','H'],'A').
p(['L','X','L','G','i','i','m','l','m'],'m').
p(['r','G','h','D','D','N','d','P'],'D').
q(['g','Q','f','g','p','c','g'],'Q').
q(['Z','V','r','C','E','o','o'],'E').
q(['g','Y','V','x','C','w','H','T','w','H','P'],'C').
q(['Q','c','E','I','d','H','Y','M','d'],'Y').
q(['f','b','L','b','M','P'],'L').
