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
my_tolower4(A,B):-downcase_atom(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_toupper6(A,B):-upcase_atom(A,B).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
my_max_list9(A,B):-max_list(A,B).
my_set10(A):-list_to_set(A,A).
my_msort11(A,B):-msort(A,B).
my_len12(A,B):-length(A,B).
my_double13(N,M):-M is 2*N,M =< 10.
my_min_list14(A,B):-min_list(A,B).
my_even15(A):-0 is A mod 2.
my_list_to_set16(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_tolower4,[char,char]).
prim(my_lowercase5,[char]).
prim(my_toupper6,[char,char]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_last8,[list(T),T]).
prim(my_max_list9,[list(int),int]).
prim(my_set10,[list(_)]).
prim(my_msort11,[list(int),list(int)]).
prim(my_len12,[list(_),int]).
prim(my_double13,[int,int]).
prim(my_min_list14,[list(int),int]).
prim(my_even15,[int]).
prim(my_list_to_set16,[list(T),list(T)]).
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
p(['z','K','T','z','W'],'z').
p(['Z','V','J','Y','Y','a','O'],'Y').
p(['N','N','Q','T','a','N','h','p','j'],'N').
p(['G','M','l','d','U','l'],'l').
p(['o','I','Y','o','y'],'o').
q(['W','z','S','K','z','J','N'],'W').
q(['B','h','d','h','u','U','F','R','p','e'],'U').
q(['p','j','V','y','E','P','w','p'],'y').
q(['h','w','T','u','E','V','p','K','K','T','P'],'E').
q(['S','C','R','C','R','L'],'L').
