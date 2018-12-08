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
my_odd3(A):-1 is A mod 2.
my_sumlist4(A,B):-sumlist(A,B).
my_flatten5(A,B):-flatten(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_even7(A):-0 is A mod 2.
my_max_list8(A,B):-max_list(A,B).
my_tolower9(A,B):-downcase_atom(A,B).
my_msort10(A,B):-msort(A,B).
my_reverse11(A,B):-reverse(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_set13(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_odd3,[int]).
prim(my_sumlist4,[list(int),int]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_uppercase6,[char]).
prim(my_even7,[int]).
prim(my_max_list8,[list(int),int]).
prim(my_tolower9,[char,char]).
prim(my_msort10,[list(int),list(int)]).
prim(my_reverse11,[list(T),list(T)]).
prim(my_double12,[int,int]).
prim(my_set13,[list(_)]).
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
p(['g','H','q','e','S','H','h'],'H').
p(['Z','Z','W','L','l','K','t'],'Z').
p(['k','m','D','x','x','H','n','H','P'],'H').
p(['t','y','y','l','I','S','I'],'I').
p(['A','Y','E','q','E'],'E').
q(['y','J','f','y','O','c'],'c').
q(['H','B','B','L','e','l','A','B','u','E','f'],'l').
q(['h','Q','h','L','l','o','P','E','J','V'],'E').
q(['R','n','[','N','N','c','j','J','e','E'],'[').
q(['n','n','S','e','v','E','W','c'],'v').
