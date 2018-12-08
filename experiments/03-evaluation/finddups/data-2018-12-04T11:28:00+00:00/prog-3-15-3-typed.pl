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
my_pred4(A,B):-succ(B,A),A > 0.
my_len5(A,B):-length(A,B).
my_set6(A):-list_to_set(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_lowercase9(A):-downcase_atom(A,A).
my_flatten10(A,B):-flatten(A,B).
my_odd11(A):-1 is A mod 2.
my_last12(A,B):-last(A,B).
my_min_list13(A,B):-min_list(A,B).
my_reverse14(A,B):-reverse(A,B).
my_succ15(A,B):-succ(A,B),B =< 10.
my_max_list16(A,B):-max_list(A,B).
my_uppercase17(A):-upcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_toupper3,[char,char]).
prim(my_pred4,[int,int]).
prim(my_len5,[list(_),int]).
prim(my_set6,[list(_)]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_double8,[int,int]).
prim(my_lowercase9,[char]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_odd11,[int]).
prim(my_last12,[list(T),T]).
prim(my_min_list13,[list(int),int]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_succ15,[int,int]).
prim(my_max_list16,[list(int),int]).
prim(my_uppercase17,[char]).
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
p(['h','h','x','F','P','M','L'],'h').
p(['N','X','v','l','v'],'v').
p(['U','f','V','f','n','Y','C','f','v','U'],'f').
p(['G','V','K','I','K','w','i','z','K'],'K').
p(['D','W','N','N','V','V','t','v'],'N').
q(['l','v','c','B','B','W','y','F','F'],'y').
q(['w','c','q','k','d','E','X','E','w'],'X').
q(['W','e','a','u','j','u','Q'],'W').
q(['o','Z','f','Y','G','k','k','D','v','p','B'],'G').
q(['G','b','R','n','R','j'],'G').
