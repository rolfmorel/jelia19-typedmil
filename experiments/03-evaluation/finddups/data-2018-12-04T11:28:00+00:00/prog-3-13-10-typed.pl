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
my_lowercase3(A):-downcase_atom(A,A).
my_set4(A):-list_to_set(A,A).
my_flatten5(A,B):-flatten(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_tolower7(A,B):-downcase_atom(A,B).
my_msort8(A,B):-msort(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_list_to_set10(A,B):-list_to_set(A,B).
my_last11(A,B):-last(A,B).
my_even12(A):-0 is A mod 2.
my_pred13(A,B):-succ(B,A),A > 0.
my_toupper14(A,B):-upcase_atom(A,B).
my_max_list15(A,B):-max_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_lowercase3,[char]).
prim(my_set4,[list(_)]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_double6,[int,int]).
prim(my_tolower7,[char,char]).
prim(my_msort8,[list(int),list(int)]).
prim(my_sumlist9,[list(int),int]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_last11,[list(T),T]).
prim(my_even12,[int]).
prim(my_pred13,[int,int]).
prim(my_toupper14,[char,char]).
prim(my_max_list15,[list(int),int]).
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
p(['J','s','j','A','A','W'],'A').
p(['Z','Z','n','g','Q'],'Z').
p(['H','L','R','q','o','J','q','X'],'q').
p(['R','C','K','C','M','Q','c','c'],'C').
p(['B','l','t','j','F','B','e','p','I'],'B').
q(['g','H','H','R','o','h','u'],'u').
q(['r','a','f','k','O','z','B','r','l','Y'],'B').
q(['m','A','a','s','z','Y','F','n','f','a','y'],'f').
q(['B','y','D','l','m','x','y'],'B').
q(['I','{','D','A','l','v','f','y','I'],'{').
