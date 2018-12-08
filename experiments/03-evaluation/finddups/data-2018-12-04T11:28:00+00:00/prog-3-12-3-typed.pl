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
my_lowercase4(A):-downcase_atom(A,A).
my_msort5(A,B):-msort(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_set7(A):-list_to_set(A,A).
my_len8(A,B):-length(A,B).
my_flatten9(A,B):-flatten(A,B).
my_min_list10(A,B):-min_list(A,B).
my_succ11(A,B):-succ(A,B),B =< 10.
my_odd12(A):-1 is A mod 2.
my_double13(N,M):-M is 2*N,M =< 10.
my_sumlist14(A,B):-sumlist(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_reverse3,[list(T),list(T)]).
prim(my_lowercase4,[char]).
prim(my_msort5,[list(int),list(int)]).
prim(my_pred6,[int,int]).
prim(my_set7,[list(_)]).
prim(my_len8,[list(_),int]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_min_list10,[list(int),int]).
prim(my_succ11,[int,int]).
prim(my_odd12,[int]).
prim(my_double13,[int,int]).
prim(my_sumlist14,[list(int),int]).
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
p(['i','I','r','Y','m','Z','Z','D'],'Z').
p(['O','O','A','b','M','F','b'],'O').
p(['f','J','F','n','f'],'f').
p(['Y','p','x','L','I','j','k','I','h'],'I').
p(['Z','x','n','s','Z','f','x'],'x').
q(['c','T','y','L','o','P','F','z','P'],'T').
q(['R','z','i','X','i','h'],'R').
q(['b','a','x','b','V','F','n'],'x').
q(['L','T','V','a','G','g','a','i'],'g').
q(['G','V','n','m','S','y','k','D','V','L'],'m').
