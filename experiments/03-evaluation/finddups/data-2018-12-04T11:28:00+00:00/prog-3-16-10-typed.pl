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
my_even4(A):-0 is A mod 2.
my_msort5(A,B):-msort(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_toupper7(A,B):-upcase_atom(A,B).
my_set8(A):-list_to_set(A,A).
my_min_list9(A,B):-min_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_succ12(A,B):-succ(A,B),B =< 10.
my_odd13(A):-1 is A mod 2.
my_list_to_set14(A,B):-list_to_set(A,B).
my_reverse15(A,B):-reverse(A,B).
my_uppercase16(A):-upcase_atom(A,A).
my_lowercase17(A):-downcase_atom(A,A).
my_flatten18(A,B):-flatten(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_tolower3,[char,char]).
prim(my_even4,[int]).
prim(my_msort5,[list(int),list(int)]).
prim(my_double6,[int,int]).
prim(my_toupper7,[char,char]).
prim(my_set8,[list(_)]).
prim(my_min_list9,[list(int),int]).
prim(my_max_list10,[list(int),int]).
prim(my_pred11,[int,int]).
prim(my_succ12,[int,int]).
prim(my_odd13,[int]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_reverse15,[list(T),list(T)]).
prim(my_uppercase16,[char]).
prim(my_lowercase17,[char]).
prim(my_flatten18,[list(list(T)),list(T)]).
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
p(['d','m','G','T','k','d','o','n'],'d').
p(['Z','r','Z','q','b','q','e','u'],'q').
p(['v','U','K','L','v'],'v').
p(['N','N','b','a','z','t','J','p','P'],'N').
p(['L','Z','L','K','H','o','P'],'L').
q(['a','v','W','c','D','D'],'c').
q(['t','h','d','P','J','P','K','p','V'],'K').
q(['n','i','J','a','y','a','j','A'],'j').
q(['B','N','C','N','l','Q','l','N'],'C').
q(['D','Y','A','D','S','D','E','L'],'S').
