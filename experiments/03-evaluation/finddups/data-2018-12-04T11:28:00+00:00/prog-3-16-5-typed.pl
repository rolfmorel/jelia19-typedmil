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
my_lowercase4(A):-downcase_atom(A,A).
my_double5(N,M):-M is 2*N,M =< 10.
my_succ6(A,B):-succ(A,B),B =< 10.
my_len7(A,B):-length(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_flatten9(A,B):-flatten(A,B).
my_msort10(A,B):-msort(A,B).
my_min_list11(A,B):-min_list(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_reverse13(A,B):-reverse(A,B).
my_uppercase14(A):-upcase_atom(A,A).
my_odd15(A):-1 is A mod 2.
my_sumlist16(A,B):-sumlist(A,B).
my_set17(A):-list_to_set(A,A).
my_list_to_set18(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_tolower3,[char,char]).
prim(my_lowercase4,[char]).
prim(my_double5,[int,int]).
prim(my_succ6,[int,int]).
prim(my_len7,[list(_),int]).
prim(my_toupper8,[char,char]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_msort10,[list(int),list(int)]).
prim(my_min_list11,[list(int),int]).
prim(my_pred12,[int,int]).
prim(my_reverse13,[list(T),list(T)]).
prim(my_uppercase14,[char]).
prim(my_odd15,[int]).
prim(my_sumlist16,[list(int),int]).
prim(my_set17,[list(_)]).
prim(my_list_to_set18,[list(T),list(T)]).
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
p(['k','C','w','X','O','w','c','j','j','k'],'w').
p(['g','d','p','p','h','v'],'p').
p(['J','d','d','R','D'],'d').
p(['A','U','a','N','A','G','p','x','d'],'A').
p(['J','a','r','Q','Y','Q','q','d'],'Q').
q(['K','g','I','w','q','I','w','Q','s'],'s').
q(['G','A','r','e','D','G','K','r','I'],'e').
q(['q','q','r','P','S','S','n'],'r').
q(['A','R','W','A','t','i','k'],'i').
q(['q','N','c','H','E','c','x','P','g'],'P').
