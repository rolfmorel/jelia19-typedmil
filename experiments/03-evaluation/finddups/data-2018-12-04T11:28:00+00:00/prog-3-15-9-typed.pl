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
my_set3(A):-list_to_set(A,A).
my_pred4(A,B):-succ(B,A),A > 0.
my_min_list5(A,B):-min_list(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_uppercase7(A):-upcase_atom(A,A).
my_last8(A,B):-last(A,B).
my_reverse9(A,B):-reverse(A,B).
my_odd10(A):-1 is A mod 2.
my_max_list11(A,B):-max_list(A,B).
my_even12(A):-0 is A mod 2.
my_toupper13(A,B):-upcase_atom(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_lowercase15(A):-downcase_atom(A,A).
my_flatten16(A,B):-flatten(A,B).
my_double17(N,M):-M is 2*N,M =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_set3,[list(_)]).
prim(my_pred4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_succ6,[int,int]).
prim(my_uppercase7,[char]).
prim(my_last8,[list(T),T]).
prim(my_reverse9,[list(T),list(T)]).
prim(my_odd10,[int]).
prim(my_max_list11,[list(int),int]).
prim(my_even12,[int]).
prim(my_toupper13,[char,char]).
prim(my_sumlist14,[list(int),int]).
prim(my_lowercase15,[char]).
prim(my_flatten16,[list(list(T)),list(T)]).
prim(my_double17,[int,int]).
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
p(['a','c','a','o','M'],'a').
p(['m','z','m','M','s','Y'],'m').
p(['y','g','N','C','y','m','X','E'],'y').
p(['w','i','d','F','R','d'],'d').
p(['N','U','R','R','a','c','Y','N','Z'],'N').
q(['H','w','J','x','Q','L','V','n','J'],'w').
q(['L','E','A','m','h','h','T','W'],'E').
q(['z','f','o','o','t','c'],'c').
q(['F','s','P','P','h','s','k','Q','F'],'k').
q(['K','o','G','K','o','r','q','P','L'],'G').
