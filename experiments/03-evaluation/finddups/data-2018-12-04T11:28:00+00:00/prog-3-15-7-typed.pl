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
my_flatten3(A,B):-flatten(A,B).
my_msort4(A,B):-msort(A,B).
my_odd5(A):-1 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_pred7(A,B):-succ(B,A),A > 0.
my_max_list8(A,B):-max_list(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_succ10(A,B):-succ(A,B),B =< 10.
my_double11(N,M):-M is 2*N,M =< 10.
my_sumlist12(A,B):-sumlist(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_lowercase14(A):-downcase_atom(A,A).
my_len15(A,B):-length(A,B).
my_last16(A,B):-last(A,B).
my_even17(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_msort4,[list(int),list(int)]).
prim(my_odd5,[int]).
prim(my_set6,[list(_)]).
prim(my_pred7,[int,int]).
prim(my_max_list8,[list(int),int]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_succ10,[int,int]).
prim(my_double11,[int,int]).
prim(my_sumlist12,[list(int),int]).
prim(my_uppercase13,[char]).
prim(my_lowercase14,[char]).
prim(my_len15,[list(_),int]).
prim(my_last16,[list(T),T]).
prim(my_even17,[int]).
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
p(['a','B','C','N','C','p','n'],'C').
p(['e','U','p','e','Q'],'e').
p(['I','H','A','I','L','m'],'I').
p(['H','E','V','O','K','E','E','E'],'E').
p(['E','E','Z','t','M','l'],'E').
q(['n','t','E','n','I','l','i','a','R','q','Q'],'i').
q(['a','w','v','a','B','m'],'v').
q(['w','C','R','I','w','N','E','K','c','X'],'N').
q(['R','W','c','r','u','u','h','h','J'],'R').
q(['z','O','Z','y','C','z','e','k','c','e','M'],'Z').
