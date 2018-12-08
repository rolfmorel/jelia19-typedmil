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
my_tolower3(A,B):-downcase_atom(A,B).
my_msort4(A,B):-msort(A,B).
my_double5(N,M):-M is 2*N,M =< 10.
my_max_list6(A,B):-max_list(A,B).
my_flatten7(A,B):-flatten(A,B).
my_set8(A):-list_to_set(A,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_len10(A,B):-length(A,B).
my_toupper11(A,B):-upcase_atom(A,B).
my_reverse12(A,B):-reverse(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_odd14(A):-1 is A mod 2.
my_uppercase15(A):-upcase_atom(A,A).
my_min_list16(A,B):-min_list(A,B).
my_lowercase17(A):-downcase_atom(A,A).
my_last18(A,B):-last(A,B).
my_succ19(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_tolower3,[char,char]).
prim(my_msort4,[list(int),list(int)]).
prim(my_double5,[int,int]).
prim(my_max_list6,[list(int),int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_set8,[list(_)]).
prim(my_pred9,[int,int]).
prim(my_len10,[list(_),int]).
prim(my_toupper11,[char,char]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_odd14,[int]).
prim(my_uppercase15,[char]).
prim(my_min_list16,[list(int),int]).
prim(my_lowercase17,[char]).
prim(my_last18,[list(T),T]).
prim(my_succ19,[int,int]).
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
p(['w','U','y','W','E','W','F'],'W').
p(['h','h','t','u','J','n','q'],'h').
p(['w','w','s','W','R'],'w').
p(['J','l','P','z','P','i','u','p','y','R'],'P').
p(['z','t','S','H','e','v','S'],'S').
q(['q','G','z','r','U','W','G'],'r').
q(['w','Q','Q','f','V','X','Q'],'w').
q(['M','K','o','L','q','H','G','o','f'],'L').
q(['l','C','u','i','P','c','W','W'],'P').
q(['z','O','z','L','y','X'],'y').
