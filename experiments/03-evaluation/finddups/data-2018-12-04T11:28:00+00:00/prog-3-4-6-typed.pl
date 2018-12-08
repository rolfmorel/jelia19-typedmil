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
my_set3(A):-list_to_set(A,A).
my_flatten4(A,B):-flatten(A,B).
my_odd5(A):-1 is A mod 2.
my_list_to_set6(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_set3,[list(_)]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_odd5,[int]).
prim(my_list_to_set6,[list(T),list(T)]).
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
p(['Z','N','Y','R','R'],'R').
p(['q','D','q','d','j','K','b','N','V','z'],'q').
p(['j','N','B','E','E','V','U','C','c','k'],'E').
p(['A','W','k','H','b','s','J','A'],'A').
p(['t','h','m','v','m','s'],'m').
q(['A','g','M','s','f','Z','S','s'],'S').
q(['d','C','z','f','k','u','f','M'],'u').
q(['F','d','Q','h','G','h','G'],'F').
q(['a','A','r','d','r','t','T'],'t').
q(['H','a','W','P','p','O','a','S','G'],'W').
