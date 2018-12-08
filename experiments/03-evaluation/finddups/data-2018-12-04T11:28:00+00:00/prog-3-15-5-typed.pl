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
my_list_to_set5(A,B):-list_to_set(A,B).
my_msort6(A,B):-msort(A,B).
my_odd7(A):-1 is A mod 2.
my_flatten8(A,B):-flatten(A,B).
my_tolower9(A,B):-downcase_atom(A,B).
my_toupper10(A,B):-upcase_atom(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_min_list12(A,B):-min_list(A,B).
my_len13(A,B):-length(A,B).
my_uppercase14(A):-upcase_atom(A,A).
my_set15(A):-list_to_set(A,A).
my_last16(A,B):-last(A,B).
my_lowercase17(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_double4,[int,int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_msort6,[list(int),list(int)]).
prim(my_odd7,[int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_tolower9,[char,char]).
prim(my_toupper10,[char,char]).
prim(my_sumlist11,[list(int),int]).
prim(my_min_list12,[list(int),int]).
prim(my_len13,[list(_),int]).
prim(my_uppercase14,[char]).
prim(my_set15,[list(_)]).
prim(my_last16,[list(T),T]).
prim(my_lowercase17,[char]).
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
p(['s','I','l','N','I'],'I').
p(['s','X','s','j','Y','s','Y','X','S','r'],'s').
p(['o','l','l','m','X','a','O','X'],'X').
p(['H','z','W','d','E','E','H'],'E').
p(['C','E','W','s','V','E'],'E').
q(['a','Q','T','R','h','h','e','o','w'],'o').
q(['g','H','R','g','a','D','f','S','B','x'],'x').
q(['h','h','e','m','B','H','S','g','b','S','v'],'g').
q(['I','n','i','F','F','l','O'],'O').
q(['l','I','a','P','Q','F','n','F','r','g','v'],'r').
