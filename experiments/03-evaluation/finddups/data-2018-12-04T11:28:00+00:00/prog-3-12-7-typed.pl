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
my_odd3(A):-1 is A mod 2.
my_min_list4(A,B):-min_list(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_double6(N,M):-M is 2*N,M =< 10.
my_tolower7(A,B):-downcase_atom(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_last9(A,B):-last(A,B).
my_reverse10(A,B):-reverse(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_len12(A,B):-length(A,B).
my_max_list13(A,B):-max_list(A,B).
my_msort14(A,B):-msort(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_odd3,[int]).
prim(my_min_list4,[list(int),int]).
prim(my_pred5,[int,int]).
prim(my_double6,[int,int]).
prim(my_tolower7,[char,char]).
prim(my_toupper8,[char,char]).
prim(my_last9,[list(T),T]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_list_to_set11,[list(T),list(T)]).
prim(my_len12,[list(_),int]).
prim(my_max_list13,[list(int),int]).
prim(my_msort14,[list(int),list(int)]).
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
p(['b','w','W','y','q','N','M','w'],'w').
p(['B','w','j','Z','j'],'j').
p(['V','v','N','X','u','A','V','Q'],'V').
p(['q','n','Z','n','P','f'],'n').
p(['T','I','X','p','v','u','f','I'],'I').
q(['C','A','C','Z','W','P'],'W').
q(['E','z','Z','O','x','z','p','y'],'Z').
q(['G','O','S','D','X','S','n','k','u','r','V'],'X').
q(['I','K','w','I','P','R'],'P').
q(['v','v','w','L','O','V'],'V').
