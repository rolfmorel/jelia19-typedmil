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
my_last3(A,B):-last(A,B).
my_tolower4(A,B):-downcase_atom(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_max_list6(A,B):-max_list(A,B).
my_odd7(A):-1 is A mod 2.
my_uppercase8(A):-upcase_atom(A,A).
my_sumlist9(A,B):-sumlist(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_toupper11(A,B):-upcase_atom(A,B).
my_set12(A):-list_to_set(A,A).
my_len13(A,B):-length(A,B).
my_reverse14(A,B):-reverse(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
my_pred16(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_tolower4,[char,char]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_max_list6,[list(int),int]).
prim(my_odd7,[int]).
prim(my_uppercase8,[char]).
prim(my_sumlist9,[list(int),int]).
prim(my_lowercase10,[char]).
prim(my_toupper11,[char,char]).
prim(my_set12,[list(_)]).
prim(my_len13,[list(_),int]).
prim(my_reverse14,[list(T),list(T)]).
prim(my_double15,[int,int]).
prim(my_pred16,[int,int]).
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
p(['d','l','l','q','r','X','l','Z'],'l').
p(['S','c','r','i','q','E','O','y','y','o'],'y').
p(['E','T','Y','S','b','F','Y','D','G','L'],'Y').
p(['H','A','q','A','q','s','S','n','S','E'],'q').
p(['s','F','C','H','s','E','a'],'s').
q(['D','E','f','j','J','t','Z','l','u','t','l'],'Z').
q(['c','I','j','Y','d','I','u','G'],'Y').
q(['Y','U','o','J','W','G','e','v','h','Y'],'o').
q(['s','g','t','N','o','z','N'],'z').
q(['j','x','W','M','J','o','w','x'],'M').
