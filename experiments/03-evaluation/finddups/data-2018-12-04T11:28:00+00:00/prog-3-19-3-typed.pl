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
my_pred3(A,B):-succ(B,A),A > 0.
my_msort4(A,B):-msort(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_last6(A,B):-last(A,B).
my_reverse7(A,B):-reverse(A,B).
my_max_list8(A,B):-max_list(A,B).
my_toupper9(A,B):-upcase_atom(A,B).
my_even10(A):-0 is A mod 2.
my_lowercase11(A):-downcase_atom(A,A).
my_odd12(A):-1 is A mod 2.
my_sumlist13(A,B):-sumlist(A,B).
my_succ14(A,B):-succ(A,B),B =< 10.
my_double15(N,M):-M is 2*N,M =< 10.
my_uppercase16(A):-upcase_atom(A,A).
my_set17(A):-list_to_set(A,A).
my_min_list18(A,B):-min_list(A,B).
my_list_to_set19(A,B):-list_to_set(A,B).
my_flatten20(A,B):-flatten(A,B).
my_len21(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_tolower5,[char,char]).
prim(my_last6,[list(T),T]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_max_list8,[list(int),int]).
prim(my_toupper9,[char,char]).
prim(my_even10,[int]).
prim(my_lowercase11,[char]).
prim(my_odd12,[int]).
prim(my_sumlist13,[list(int),int]).
prim(my_succ14,[int,int]).
prim(my_double15,[int,int]).
prim(my_uppercase16,[char]).
prim(my_set17,[list(_)]).
prim(my_min_list18,[list(int),int]).
prim(my_list_to_set19,[list(T),list(T)]).
prim(my_flatten20,[list(list(T)),list(T)]).
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
p(['y','U','t','e','R','O','K','Q','t'],'t').
p(['O','a','x','p','O','n','V','e','w'],'O').
p(['J','R','l','z','R','K','M','W','p'],'R').
p(['Y','h','C','T','h'],'h').
p(['b','b','H','l','l','x','y'],'b').
q(['P','t','u','s','h','P'],'h').
q(['m','O','q','S','P','S','q','t'],'O').
q(['V','H','J','W','I','H','c','Z','C','U'],'U').
q(['I','S','I','s','q','E','A','I'],'q').
q(['l','x','k','S','B','l','J'],'B').
