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
my_lowercase4(A):-downcase_atom(A,A).
my_list_to_set5(A,B):-list_to_set(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_min_list10(A,B):-min_list(A,B).
my_msort11(A,B):-msort(A,B).
my_even12(A):-0 is A mod 2.
my_odd13(A):-1 is A mod 2.
my_tolower14(A,B):-downcase_atom(A,B).
my_toupper15(A,B):-upcase_atom(A,B).
my_flatten16(A,B):-flatten(A,B).
my_succ17(A,B):-succ(A,B),B =< 10.
my_double18(N,M):-M is 2*N,M =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_lowercase4,[char]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_uppercase6,[char]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_last8,[list(T),T]).
prim(my_sumlist9,[list(int),int]).
prim(my_min_list10,[list(int),int]).
prim(my_msort11,[list(int),list(int)]).
prim(my_even12,[int]).
prim(my_odd13,[int]).
prim(my_tolower14,[char,char]).
prim(my_toupper15,[char,char]).
prim(my_flatten16,[list(list(T)),list(T)]).
prim(my_succ17,[int,int]).
prim(my_double18,[int,int]).
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
p(['p','f','s','X','h','X'],'X').
p(['k','r','P','G','r','Y','T','b'],'r').
p(['V','a','l','B','B','G','g','H','v'],'B').
p(['g','g','I','R','G','B','E','d'],'g').
p(['O','K','d','r','J','Q','O','s','O'],'O').
q(['o','u','S','y','d','S','w','O'],'w').
q(['i','S','d','d','z','q','c','Q'],'z').
q(['R','l','q','e','M','M','L'],'q').
q(['K','{','K','F','N','r','j','K','p','c','H'],'{').
q(['j','z','B','H','q','F','T','c','p','c'],'H').
