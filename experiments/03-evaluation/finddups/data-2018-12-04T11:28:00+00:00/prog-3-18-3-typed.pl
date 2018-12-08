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
my_msort4(A,B):-msort(A,B).
my_uppercase5(A):-upcase_atom(A,A).
my_reverse6(A,B):-reverse(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_sumlist8(A,B):-sumlist(A,B).
my_odd9(A):-1 is A mod 2.
my_last10(A,B):-last(A,B).
my_lowercase11(A):-downcase_atom(A,A).
my_min_list12(A,B):-min_list(A,B).
my_even13(A):-0 is A mod 2.
my_toupper14(A,B):-upcase_atom(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
my_succ16(A,B):-succ(A,B),B =< 10.
my_flatten17(A,B):-flatten(A,B).
my_max_list18(A,B):-max_list(A,B).
my_list_to_set19(A,B):-list_to_set(A,B).
my_set20(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_msort4,[list(int),list(int)]).
prim(my_uppercase5,[char]).
prim(my_reverse6,[list(T),list(T)]).
prim(my_pred7,[int,int]).
prim(my_sumlist8,[list(int),int]).
prim(my_odd9,[int]).
prim(my_last10,[list(T),T]).
prim(my_lowercase11,[char]).
prim(my_min_list12,[list(int),int]).
prim(my_even13,[int]).
prim(my_toupper14,[char,char]).
prim(my_double15,[int,int]).
prim(my_succ16,[int,int]).
prim(my_flatten17,[list(list(T)),list(T)]).
prim(my_max_list18,[list(int),int]).
prim(my_list_to_set19,[list(T),list(T)]).
prim(my_set20,[list(_)]).
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
p(['Y','t','L','L','O','A','I','q','t'],'t').
p(['J','D','J','a','U','y','t','N'],'J').
p(['J','P','N','C','d','u','P','V','C'],'P').
p(['Q','W','o','W','M','p','F'],'W').
p(['w','z','Z','c','n','o','X','G','G','L'],'G').
q(['P','z','g','E','s','z','K','B','o'],'g').
q(['j','T','O','R','s','e','z','Z','p','X','p'],'Z').
q(['K','M','x','B','M','S','i'],'x').
q(['N','n','Z','m','i','k','l','E','s','n','U'],'m').
q(['C','U','b','e','d','m','d','L','C','J'],'m').
