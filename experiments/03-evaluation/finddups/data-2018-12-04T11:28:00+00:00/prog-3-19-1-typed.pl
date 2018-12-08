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
my_max_list3(A,B):-max_list(A,B).
my_msort4(A,B):-msort(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_reverse6(A,B):-reverse(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_set8(A):-list_to_set(A,A).
my_double9(N,M):-M is 2*N,M =< 10.
my_sumlist10(A,B):-sumlist(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_tolower12(A,B):-downcase_atom(A,B).
my_flatten13(A,B):-flatten(A,B).
my_len14(A,B):-length(A,B).
my_toupper15(A,B):-upcase_atom(A,B).
my_min_list16(A,B):-min_list(A,B).
my_uppercase17(A):-upcase_atom(A,A).
my_odd18(A):-1 is A mod 2.
my_last19(A,B):-last(A,B).
my_even20(A):-0 is A mod 2.
my_succ21(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_max_list3,[list(int),int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_pred5,[int,int]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_lowercase7,[char]).
prim(my_set8,[list(_)]).
prim(my_double9,[int,int]).
prim(my_sumlist10,[list(int),int]).
prim(my_list_to_set11,[list(T),list(T)]).
prim(my_tolower12,[char,char]).
prim(my_flatten13,[list(list(T)),list(T)]).
prim(my_len14,[list(_),int]).
prim(my_toupper15,[char,char]).
prim(my_min_list16,[list(int),int]).
prim(my_uppercase17,[char]).
prim(my_odd18,[int]).
prim(my_last19,[list(T),T]).
prim(my_even20,[int]).
prim(my_succ21,[int,int]).
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
p(['a','P','h','u','a','q','E','R','R'],'R').
p(['p','b','s','L','s'],'s').
p(['x','G','T','x','q'],'x').
p(['o','q','c','P','c','s','d','o','f','O'],'c').
p(['s','L','V','x','s','T','W','b','m'],'s').
q(['L','E','p','B','U','T','Z','T'],'Z').
q(['a','r','I','U','p','O','y','c','y','r','a'],'O').
q(['B','F','u','o','C','a','S','C','p'],'B').
q(['C','W','a','b','N','R','i','R'],'N').
q(['f','j','b','o','K','n','f','H','s'],'n').
