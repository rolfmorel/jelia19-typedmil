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
my_msort3(A,B):-msort(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_flatten8(A,B):-flatten(A,B).
my_len9(A,B):-length(A,B).
my_reverse10(A,B):-reverse(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_succ13(A,B):-succ(A,B),B =< 10.
my_toupper14(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_msort3,[list(int),list(int)]).
prim(my_lowercase4,[char]).
prim(my_last5,[list(T),T]).
prim(my_min_list6,[list(int),int]).
prim(my_sumlist7,[list(int),int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_len9,[list(_),int]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_tolower11,[char,char]).
prim(my_pred12,[int,int]).
prim(my_succ13,[int,int]).
prim(my_toupper14,[char,char]).
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
p(['e','s','E','m','P','E'],'E').
p(['K','k','u','U','f','K','X','G','t','R'],'K').
p(['k','y','Z','E','E'],'E').
p(['X','t','X','W','f'],'X').
p(['x','Z','A','Z','s','w','V','d','R'],'Z').
q(['m','y','u','P','w','w','f','b','p'],'u').
q(['m','x','C','E','C','j','p','J','f','h'],'m').
q(['z','y','c','e','g','[','c','u','B','m','A'],'[').
q(['j','a','M','a','q','q','u','a','I'],'u').
q(['[','p','T','T','f','G','i'],'[').
