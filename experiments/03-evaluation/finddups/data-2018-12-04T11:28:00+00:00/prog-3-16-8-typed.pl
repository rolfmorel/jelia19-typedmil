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
my_succ4(A,B):-succ(A,B),B =< 10.
my_reverse5(A,B):-reverse(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_tolower7(A,B):-downcase_atom(A,B).
my_min_list8(A,B):-min_list(A,B).
my_odd9(A):-1 is A mod 2.
my_sumlist10(A,B):-sumlist(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_msort12(A,B):-msort(A,B).
my_even13(A):-0 is A mod 2.
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_set16(A):-list_to_set(A,A).
my_double17(N,M):-M is 2*N,M =< 10.
my_pred18(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_toupper3,[char,char]).
prim(my_succ4,[int,int]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_uppercase6,[char]).
prim(my_tolower7,[char,char]).
prim(my_min_list8,[list(int),int]).
prim(my_odd9,[int]).
prim(my_sumlist10,[list(int),int]).
prim(my_list_to_set11,[list(T),list(T)]).
prim(my_msort12,[list(int),list(int)]).
prim(my_even13,[int]).
prim(my_len14,[list(_),int]).
prim(my_last15,[list(T),T]).
prim(my_set16,[list(_)]).
prim(my_double17,[int,int]).
prim(my_pred18,[int,int]).
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
p(['i','h','f','z','c','z','z','K','F','I'],'z').
p(['v','n','w','n','R','U','P'],'n').
p(['z','D','u','z','D','G'],'z').
p(['a','R','p','B','E','V','a','l','t','p'],'p').
p(['M','M','E','d','v','p','P'],'M').
q(['F','O','R','c','v','z','R','V','X','f'],'c').
q(['N','v','z','z','i','s'],'N').
q(['e','o','U','p','L','S','k','C','l','o'],'U').
q(['L','n','d','L','j','[','O'],'[').
q(['u','O','d','E','Z','Q','X','d','D','k','Y'],'Z').
