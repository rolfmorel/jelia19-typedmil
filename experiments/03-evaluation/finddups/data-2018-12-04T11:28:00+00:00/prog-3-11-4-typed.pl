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
my_sumlist3(A,B):-sumlist(A,B).
my_tolower4(A,B):-downcase_atom(A,B).
my_last5(A,B):-last(A,B).
my_flatten6(A,B):-flatten(A,B).
my_max_list7(A,B):-max_list(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_msort11(A,B):-msort(A,B).
my_set12(A):-list_to_set(A,A).
my_even13(A):-0 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_tolower4,[char,char]).
prim(my_last5,[list(T),T]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_max_list7,[list(int),int]).
prim(my_toupper8,[char,char]).
prim(my_list_to_set9,[list(T),list(T)]).
prim(my_lowercase10,[char]).
prim(my_msort11,[list(int),list(int)]).
prim(my_set12,[list(_)]).
prim(my_even13,[int]).
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
p(['m','t','t','V','F','v'],'t').
p(['u','Y','u','d','t'],'u').
p(['c','S','E','r','Q','E','k','F'],'E').
p(['K','L','S','s','l','K','L'],'K').
p(['n','a','a','a','s'],'a').
q(['v','C','y','q','c','n','S','t','c'],'v').
q(['J','q','r','C','V','X','V'],'X').
q(['Z','y','Z','W','c','k'],'W').
q(['s','I','a','p','F','V','F','r','Z','M'],'p').
q(['u','j','R','k','k','P'],'j').
