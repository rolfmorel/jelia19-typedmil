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
my_max_list4(A,B):-max_list(A,B).
my_flatten5(A,B):-flatten(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_succ7(A,B):-succ(A,B),B =< 10.
my_even8(A):-0 is A mod 2.
my_set9(A):-list_to_set(A,A).
my_msort10(A,B):-msort(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_uppercase12(A):-upcase_atom(A,A).
my_sumlist13(A,B):-sumlist(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_max_list4,[list(int),int]).
prim(my_flatten5,[list(list(T)),list(T)]).
prim(my_pred6,[int,int]).
prim(my_succ7,[int,int]).
prim(my_even8,[int]).
prim(my_set9,[list(_)]).
prim(my_msort10,[list(int),list(int)]).
prim(my_tolower11,[char,char]).
prim(my_uppercase12,[char]).
prim(my_sumlist13,[list(int),int]).
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
p(['S','Q','v','I','F','v','X'],'v').
p(['h','R','R','r','o','u','T','o','i'],'o').
p(['H','X','d','K','T','b','q','Q','t','T'],'T').
p(['s','u','P','N','h','C','S','S','F','e'],'S').
p(['Q','d','d','J','b','e'],'d').
q(['c','o','X','I','m','E','E','V','Y'],'I').
q(['v','R','b','F','E','z','E'],'z').
q(['G','o','p','S','K','E','b','M','t','r','K'],'t').
q(['L','e','Z','Z','m','o','Y'],'L').
q(['D','s','z','Q','Q','b'],'s').
