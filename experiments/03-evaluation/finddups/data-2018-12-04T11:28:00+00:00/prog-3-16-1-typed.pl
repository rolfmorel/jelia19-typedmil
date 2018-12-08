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
my_flatten4(A,B):-flatten(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_len6(A,B):-length(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_even8(A):-0 is A mod 2.
my_odd9(A):-1 is A mod 2.
my_toupper10(A,B):-upcase_atom(A,B).
my_reverse11(A,B):-reverse(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_set13(A):-list_to_set(A,A).
my_double14(N,M):-M is 2*N,M =< 10.
my_max_list15(A,B):-max_list(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_msort17(A,B):-msort(A,B).
my_lowercase18(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_last3,[list(T),T]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_tolower5,[char,char]).
prim(my_len6,[list(_),int]).
prim(my_pred7,[int,int]).
prim(my_even8,[int]).
prim(my_odd9,[int]).
prim(my_toupper10,[char,char]).
prim(my_reverse11,[list(T),list(T)]).
prim(my_list_to_set12,[list(T),list(T)]).
prim(my_set13,[list(_)]).
prim(my_double14,[int,int]).
prim(my_max_list15,[list(int),int]).
prim(my_sumlist16,[list(int),int]).
prim(my_msort17,[list(int),list(int)]).
prim(my_lowercase18,[char]).
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
p(['q','Q','U','N','N','F','n'],'N').
p(['y','N','U','u','u'],'u').
p(['n','w','V','V','V'],'V').
p(['N','G','F','s','z','f','F','g'],'F').
p(['I','T','R','R','g','w'],'R').
q(['J','r','r','F','e','Y','z'],'J').
q(['t','Q','K','n','I','x','u','K'],'n').
q(['E','n','I','k','u','M','b','R','n','S'],'k').
q(['C','C','j','Y','Y','h','o','c'],'h').
q(['t','p','T','Q','l','T','c','b','Z','c'],'Q').
