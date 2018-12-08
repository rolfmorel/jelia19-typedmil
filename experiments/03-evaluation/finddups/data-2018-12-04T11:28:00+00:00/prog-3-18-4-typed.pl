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
my_set3(A):-list_to_set(A,A).
my_double4(N,M):-M is 2*N,M =< 10.
my_tolower5(A,B):-downcase_atom(A,B).
my_flatten6(A,B):-flatten(A,B).
my_len7(A,B):-length(A,B).
my_min_list8(A,B):-min_list(A,B).
my_msort9(A,B):-msort(A,B).
my_max_list10(A,B):-max_list(A,B).
my_last11(A,B):-last(A,B).
my_reverse12(A,B):-reverse(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_even14(A):-0 is A mod 2.
my_lowercase15(A):-downcase_atom(A,A).
my_pred16(A,B):-succ(B,A),A > 0.
my_succ17(A,B):-succ(A,B),B =< 10.
my_list_to_set18(A,B):-list_to_set(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_uppercase20(A):-upcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_set3,[list(_)]).
prim(my_double4,[int,int]).
prim(my_tolower5,[char,char]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_len7,[list(_),int]).
prim(my_min_list8,[list(int),int]).
prim(my_msort9,[list(int),list(int)]).
prim(my_max_list10,[list(int),int]).
prim(my_last11,[list(T),T]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_toupper13,[char,char]).
prim(my_even14,[int]).
prim(my_lowercase15,[char]).
prim(my_pred16,[int,int]).
prim(my_succ17,[int,int]).
prim(my_list_to_set18,[list(T),list(T)]).
prim(my_sumlist19,[list(int),int]).
prim(my_uppercase20,[char]).
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
p(['T','C','U','k','b','G','G','a','K','f'],'G').
p(['J','D','I','Z','J','d','J'],'J').
p(['H','U','h','U','w','d','w','S'],'U').
p(['C','X','V','E','O','P','X','u','b','s'],'X').
p(['W','q','w','j','K','w','P'],'w').
q(['Y','M','g','C','z','s','w','w'],'C').
q(['J','f','z','R','C','R','s','d','r','J'],'f').
q(['X','I','p','A','m','p'],'X').
q(['i','G','e','P','U','P','z','N','Q'],'i').
q(['s','j','Q','P','N','t','v','y','y'],'t').
