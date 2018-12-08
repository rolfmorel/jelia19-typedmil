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
my_max_list4(A,B):-max_list(A,B).
my_set5(A):-list_to_set(A,A).
my_len6(A,B):-length(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_reverse8(A,B):-reverse(A,B).
my_uppercase9(A):-upcase_atom(A,A).
my_even10(A):-0 is A mod 2.
my_pred11(A,B):-succ(B,A),A > 0.
my_min_list12(A,B):-min_list(A,B).
my_flatten13(A,B):-flatten(A,B).
my_succ14(A,B):-succ(A,B),B =< 10.
my_sumlist15(A,B):-sumlist(A,B).
my_odd16(A):-1 is A mod 2.
my_double17(N,M):-M is 2*N,M =< 10.
my_last18(A,B):-last(A,B).
my_tolower19(A,B):-downcase_atom(A,B).
my_msort20(A,B):-msort(A,B).
my_lowercase21(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_toupper3,[char,char]).
prim(my_max_list4,[list(int),int]).
prim(my_set5,[list(_)]).
prim(my_len6,[list(_),int]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_reverse8,[list(T),list(T)]).
prim(my_uppercase9,[char]).
prim(my_even10,[int]).
prim(my_pred11,[int,int]).
prim(my_min_list12,[list(int),int]).
prim(my_flatten13,[list(list(T)),list(T)]).
prim(my_succ14,[int,int]).
prim(my_sumlist15,[list(int),int]).
prim(my_odd16,[int]).
prim(my_double17,[int,int]).
prim(my_last18,[list(T),T]).
prim(my_tolower19,[char,char]).
prim(my_msort20,[list(int),list(int)]).
prim(my_lowercase21,[char]).
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
p(['d','V','R','a','H','R','R','B','D'],'R').
p(['o','X','v','H','o','v','S','n'],'o').
p(['r','D','u','g','r'],'r').
p(['N','I','t','Q','x','t','P','E','F','T'],'t').
p(['V','h','o','o','O','M'],'o').
q(['X','k','v','X','d','n','S'],'k').
q(['c','o','J','j','r','q','j','X'],'J').
q(['{','O','P','f','L','D','L','Y'],'{').
q(['E','B','R','r','N','B','w','q','X','a'],'R').
q(['G','c','c','Q','s','c'],'s').
