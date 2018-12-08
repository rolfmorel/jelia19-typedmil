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
my_len3(A,B):-length(A,B).
my_last4(A,B):-last(A,B).
my_reverse5(A,B):-reverse(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_double7(N,M):-M is 2*N,M =< 10.
my_flatten8(A,B):-flatten(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_odd10(A):-1 is A mod 2.
my_max_list11(A,B):-max_list(A,B).
my_even12(A):-0 is A mod 2.
my_uppercase13(A):-upcase_atom(A,A).
my_lowercase14(A):-downcase_atom(A,A).
my_tolower15(A,B):-downcase_atom(A,B).
my_msort16(A,B):-msort(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_set18(A):-list_to_set(A,A).
my_list_to_set19(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_last4,[list(T),T]).
prim(my_reverse5,[list(T),list(T)]).
prim(my_succ6,[int,int]).
prim(my_double7,[int,int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_pred9,[int,int]).
prim(my_odd10,[int]).
prim(my_max_list11,[list(int),int]).
prim(my_even12,[int]).
prim(my_uppercase13,[char]).
prim(my_lowercase14,[char]).
prim(my_tolower15,[char,char]).
prim(my_msort16,[list(int),list(int)]).
prim(my_sumlist17,[list(int),int]).
prim(my_set18,[list(_)]).
prim(my_list_to_set19,[list(T),list(T)]).
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
p(['R','r','H','F','r','X','V','e'],'r').
p(['j','i','K','u','J','B','U','m','b','i'],'i').
p(['i','J','t','M','u','L','n','M','w'],'M').
p(['t','P','b','c','C','C','Y'],'C').
p(['X','Z','y','F','U','Z','S'],'Z').
q(['T','V','g','h','i','q','j','H','G','W','g'],'G').
q(['f','D','m','n','m','y','a','E','m'],'n').
q(['p','V','i','V','M','n','X','L'],'X').
q(['B','F','{','n','e','X','n','S'],'{').
q(['P','M','z','s','c','s','k','E'],'M').
