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
my_succ3(A,B):-succ(A,B),B =< 10.
my_flatten4(A,B):-flatten(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_uppercase6(A):-upcase_atom(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_max_list8(A,B):-max_list(A,B).
my_set9(A):-list_to_set(A,A).
my_reverse10(A,B):-reverse(A,B).
my_even11(A):-0 is A mod 2.
my_msort12(A,B):-msort(A,B).
my_min_list13(A,B):-min_list(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_tolower15(A,B):-downcase_atom(A,B).
my_double16(N,M):-M is 2*N,M =< 10.
my_toupper17(A,B):-upcase_atom(A,B).
my_sumlist18(A,B):-sumlist(A,B).
my_last19(A,B):-last(A,B).
my_odd20(A):-1 is A mod 2.
my_len21(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_succ3,[int,int]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_lowercase5,[char]).
prim(my_uppercase6,[char]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_max_list8,[list(int),int]).
prim(my_set9,[list(_)]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_even11,[int]).
prim(my_msort12,[list(int),list(int)]).
prim(my_min_list13,[list(int),int]).
prim(my_pred14,[int,int]).
prim(my_tolower15,[char,char]).
prim(my_double16,[int,int]).
prim(my_toupper17,[char,char]).
prim(my_sumlist18,[list(int),int]).
prim(my_last19,[list(T),T]).
prim(my_odd20,[int]).
prim(my_len21,[list(_),int]).
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
p(['V','V','N','A','B','A'],'V').
p(['R','M','R','n','X','R','T','q','b'],'R').
p(['a','o','z','z','U','O','l','o','Y'],'o').
p(['G','y','O','Q','d','G','Q','z'],'G').
p(['u','u','G','z','y','I','D','W','d','l'],'u').
q(['r','r','W','s','A','r','W'],'s').
q(['a','F','a','n','P','E','D','{'],'{').
q(['u','u','e','Z','s','c','i'],'i').
q(['C','t','S','e','e','e','y','t'],'C').
q(['t','u','o','i','w','K','I','w','J'],'I').
