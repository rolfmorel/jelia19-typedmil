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
my_uppercase3(A):-upcase_atom(A,A).
my_double4(N,M):-M is 2*N,M =< 10.
my_sumlist5(A,B):-sumlist(A,B).
my_even6(A):-0 is A mod 2.
my_pred7(A,B):-succ(B,A),A > 0.
my_len8(A,B):-length(A,B).
my_min_list9(A,B):-min_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_toupper11(A,B):-upcase_atom(A,B).
my_last12(A,B):-last(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_msort14(A,B):-msort(A,B).
my_reverse15(A,B):-reverse(A,B).
my_set16(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_uppercase3,[char]).
prim(my_double4,[int,int]).
prim(my_sumlist5,[list(int),int]).
prim(my_even6,[int]).
prim(my_pred7,[int,int]).
prim(my_len8,[list(_),int]).
prim(my_min_list9,[list(int),int]).
prim(my_max_list10,[list(int),int]).
prim(my_toupper11,[char,char]).
prim(my_last12,[list(T),T]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_msort14,[list(int),list(int)]).
prim(my_reverse15,[list(T),list(T)]).
prim(my_set16,[list(_)]).
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
p(['y','p','o','Q','I','o','S','c','n','r'],'o').
p(['Z','V','X','d','V','K','k','N','d','k'],'V').
p(['g','V','l','T','V','W','o','W','l','D'],'l').
p(['o','Z','X','E','G','y','y','a','L','c'],'y').
p(['L','q','p','p','x','J'],'p').
q(['U','n','k','N','o','p','E','O','p'],'O').
q(['l','l','p','j','Q','r','e','V'],'p').
q(['G','J','I','K','i','z','l','J','b'],'K').
q(['q','C','B','q','N','S','h'],'C').
q(['D','y','r','F','D','d'],'y').
