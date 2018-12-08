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
my_even3(A):-0 is A mod 2.
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_max_list7(A,B):-max_list(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_even3,[int]).
prim(my_min_list4,[list(int),int]).
prim(my_sumlist5,[list(int),int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_max_list7,[list(int),int]).
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
p(['K','j','d','y','x','g','t','k','d'],'d').
p(['t','e','E','T','X','t','b'],'t').
p(['C','x','i','m','C','n','Y'],'C').
p(['i','V','n','Z','i','I','b'],'i').
p(['g','t','w','R','a','a','a','B'],'a').
q(['x','x','i','D','H','M'],'H').
q(['g','P','E','H','H','Q','K','G'],'P').
q(['q','L','e','u','S','h','y','L','w'],'y').
q(['a','y','Y','a','D','Z','P'],'Y').
q(['W','C','k','l','{','e','L','W','I'],'{').
