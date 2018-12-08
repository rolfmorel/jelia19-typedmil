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
my_reverse3(A,B):-reverse(A,B).
my_odd4(A):-1 is A mod 2.
my_min_list5(A,B):-min_list(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_last7(A,B):-last(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_set10(A):-list_to_set(A,A).
my_double11(N,M):-M is 2*N,M =< 10.
my_toupper12(A,B):-upcase_atom(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_max_list14(A,B):-max_list(A,B).
my_len15(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_reverse3,[list(T),list(T)]).
prim(my_odd4,[int]).
prim(my_min_list5,[list(int),int]).
prim(my_lowercase6,[char]).
prim(my_last7,[list(T),T]).
prim(my_list_to_set8,[list(T),list(T)]).
prim(my_succ9,[int,int]).
prim(my_set10,[list(_)]).
prim(my_double11,[int,int]).
prim(my_toupper12,[char,char]).
prim(my_uppercase13,[char]).
prim(my_max_list14,[list(int),int]).
prim(my_len15,[list(_),int]).
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
p(['E','C','p','l','y','C'],'C').
p(['a','Q','I','g','L','Z','s','g'],'g').
p(['p','l','K','g','G','y','y'],'y').
p(['q','o','D','b','S','J','o'],'o').
p(['X','a','p','L','b','s','n','w','g','g'],'g').
q(['o','V','V','z','s','f'],'o').
q(['m','m','v','F','U','x','Y','Q'],'v').
q(['D','y','f','R','k','c','f'],'k').
q(['i','B','F','j','F','T','h','o','b','T'],'i').
q(['R','i','y','d','Z','y','E','r'],'Z').
