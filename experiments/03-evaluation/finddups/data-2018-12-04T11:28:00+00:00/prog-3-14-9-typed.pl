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
my_succ3(A,B):-succ(A,B),B =< 10.
my_min_list4(A,B):-min_list(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_max_list7(A,B):-max_list(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_msort9(A,B):-msort(A,B).
my_odd10(A):-1 is A mod 2.
my_set11(A):-list_to_set(A,A).
my_double12(N,M):-M is 2*N,M =< 10.
my_uppercase13(A):-upcase_atom(A,A).
my_reverse14(A,B):-reverse(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_lowercase16(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_min_list4,[list(int),int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_pred6,[int,int]).
prim(my_max_list7,[list(int),int]).
prim(my_tolower8,[char,char]).
prim(my_msort9,[list(int),list(int)]).
prim(my_odd10,[int]).
prim(my_set11,[list(_)]).
prim(my_double12,[int,int]).
prim(my_uppercase13,[char]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_sumlist15,[list(int),int]).
prim(my_lowercase16,[char]).
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
p(['P','f','W','i','D','W','W'],'W').
p(['h','G','l','i','v','J','i'],'i').
p(['w','q','i','i','w','h','w','M'],'i').
p(['Q','N','f','u','N','s'],'N').
p(['I','O','k','s','x','g','k'],'k').
q(['P','U','p','U','N','U','R','Q'],'p').
q(['u','u','e','r','N','n','t','c'],'t').
q(['c','K','Y','t','q','z','Y','z'],'c').
q(['g','j','V','W','P','i','W'],'V').
q(['I','h','o','R','u','T','T'],'u').
