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
my_flatten4(A,B):-flatten(A,B).
my_uppercase5(A):-upcase_atom(A,A).
my_even6(A):-0 is A mod 2.
my_toupper7(A,B):-upcase_atom(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_lowercase9(A):-downcase_atom(A,A).
my_msort10(A,B):-msort(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_min_list12(A,B):-min_list(A,B).
my_set13(A):-list_to_set(A,A).
my_len14(A,B):-length(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_double16(N,M):-M is 2*N,M =< 10.
my_succ17(A,B):-succ(A,B),B =< 10.
my_odd18(A):-1 is A mod 2.
my_max_list19(A,B):-max_list(A,B).
my_last20(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_reverse3,[list(T),list(T)]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_uppercase5,[char]).
prim(my_even6,[int]).
prim(my_toupper7,[char,char]).
prim(my_pred8,[int,int]).
prim(my_lowercase9,[char]).
prim(my_msort10,[list(int),list(int)]).
prim(my_tolower11,[char,char]).
prim(my_min_list12,[list(int),int]).
prim(my_set13,[list(_)]).
prim(my_len14,[list(_),int]).
prim(my_sumlist15,[list(int),int]).
prim(my_double16,[int,int]).
prim(my_succ17,[int,int]).
prim(my_odd18,[int]).
prim(my_max_list19,[list(int),int]).
prim(my_last20,[list(T),T]).
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
p(['n','Z','w','Y','o','X','p','t','o'],'o').
p(['p','Q','R','p','v','v'],'p').
p(['O','s','C','O','e','A'],'O').
p(['V','R','h','M','k','h','X'],'h').
p(['k','H','P','R','k','K','n'],'k').
q(['H','F','w','R','Q','q','q'],'H').
q(['N','y','y','X','x','Y','d','N','c','s','I'],'d').
q(['N','E','c','V','p','N','v'],'p').
q(['J','o','j','v','c','v'],'c').
q(['f','B','y','q','B','t'],'y').
