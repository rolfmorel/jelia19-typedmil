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
my_pred3(A,B):-succ(B,A),A > 0.
my_odd4(A):-1 is A mod 2.
my_set5(A):-list_to_set(A,A).
my_min_list6(A,B):-min_list(A,B).
my_flatten7(A,B):-flatten(A,B).
my_msort8(A,B):-msort(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_sumlist10(A,B):-sumlist(A,B).
my_uppercase11(A):-upcase_atom(A,A).
my_reverse12(A,B):-reverse(A,B).
my_double13(N,M):-M is 2*N,M =< 10.
my_list_to_set14(A,B):-list_to_set(A,B).
my_tolower15(A,B):-downcase_atom(A,B).
my_even16(A):-0 is A mod 2.
my_last17(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_odd4,[int]).
prim(my_set5,[list(_)]).
prim(my_min_list6,[list(int),int]).
prim(my_flatten7,[list(list(T)),list(T)]).
prim(my_msort8,[list(int),list(int)]).
prim(my_toupper9,[char,char]).
prim(my_sumlist10,[list(int),int]).
prim(my_uppercase11,[char]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_double13,[int,int]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_tolower15,[char,char]).
prim(my_even16,[int]).
prim(my_last17,[list(T),T]).
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
p(['R','q','e','E','A','q','O','g','d','G'],'q').
p(['f','w','n','f','q'],'f').
p(['B','R','r','z','O','R'],'R').
p(['V','N','m','W','W'],'W').
p(['B','A','D','x','B','v','M','u'],'B').
q(['Z','C','Q','G','v','p','v','y'],'p').
q(['k','Y','K','D','Y','D','A','W'],'k').
q(['h','J','c','g','h','c','x'],'x').
q(['q','S','t','q','O','T','T','F','J','F','a'],'a').
q(['Q','t','j','B','Y','v','m','k','v'],'m').
