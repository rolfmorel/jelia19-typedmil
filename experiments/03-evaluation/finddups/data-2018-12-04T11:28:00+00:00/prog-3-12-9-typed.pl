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
my_max_list3(A,B):-max_list(A,B).
my_flatten4(A,B):-flatten(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_min_list7(A,B):-min_list(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_last9(A,B):-last(A,B).
my_toupper10(A,B):-upcase_atom(A,B).
my_reverse11(A,B):-reverse(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_uppercase13(A):-upcase_atom(A,A).
my_len14(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_max_list3,[list(int),int]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_lowercase6,[char]).
prim(my_min_list7,[list(int),int]).
prim(my_pred8,[int,int]).
prim(my_last9,[list(T),T]).
prim(my_toupper10,[char,char]).
prim(my_reverse11,[list(T),list(T)]).
prim(my_succ12,[int,int]).
prim(my_uppercase13,[char]).
prim(my_len14,[list(_),int]).
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
p(['D','x','N','i','V','f','W','D','D','s'],'D').
p(['y','M','s','f','p','M','m','l','Q','s'],'M').
p(['p','d','l','x','V','p','V'],'V').
p(['R','R','J','u','F','J','L','v','W'],'R').
p(['s','s','A','Z','c','S','J'],'s').
q(['X','t','i','X','z','j'],'t').
q(['t','j','e','v','d','N','e','I'],'I').
q(['F','e','q','R','T','j','j'],'R').
q(['u','s','X','u','i','I'],'X').
q(['Y','n','Y','R','F','H','c','E'],'F').
