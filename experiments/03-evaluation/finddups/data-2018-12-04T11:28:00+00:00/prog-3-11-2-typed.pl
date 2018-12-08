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
my_flatten3(A,B):-flatten(A,B).
my_msort4(A,B):-msort(A,B).
my_max_list5(A,B):-max_list(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_toupper7(A,B):-upcase_atom(A,B).
my_last8(A,B):-last(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_even10(A):-0 is A mod 2.
my_min_list11(A,B):-min_list(A,B).
my_succ12(A,B):-succ(A,B),B =< 10.
my_reverse13(A,B):-reverse(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_msort4,[list(int),list(int)]).
prim(my_max_list5,[list(int),int]).
prim(my_lowercase6,[char]).
prim(my_toupper7,[char,char]).
prim(my_last8,[list(T),T]).
prim(my_sumlist9,[list(int),int]).
prim(my_even10,[int]).
prim(my_min_list11,[list(int),int]).
prim(my_succ12,[int,int]).
prim(my_reverse13,[list(T),list(T)]).
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
p(['O','p','H','H','V','Q'],'H').
p(['Z','e','E','Q','Q','O'],'Q').
p(['t','G','v','h','o','v','G','S','m'],'G').
p(['o','G','T','G','X','y','Z','Z','x'],'G').
p(['T','S','G','d','d'],'d').
q(['A','u','O','h','u','t','Z','t'],'Z').
q(['K','e','S','t','s','I','s'],'I').
q(['s','T','n','O','h','n','A','U','l'],'T').
q(['K','v','K','d','D','u','b','z','z','R'],'v').
q(['l','n','M','T','Q','F','Q','B','h','L','G'],'l').
