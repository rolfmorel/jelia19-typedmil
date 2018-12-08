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
my_odd4(A):-1 is A mod 2.
my_len5(A,B):-length(A,B).
my_flatten6(A,B):-flatten(A,B).
my_set7(A):-list_to_set(A,A).
my_uppercase8(A):-upcase_atom(A,A).
my_msort9(A,B):-msort(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_max_list3,[list(int),int]).
prim(my_odd4,[int]).
prim(my_len5,[list(_),int]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_set7,[list(_)]).
prim(my_uppercase8,[char]).
prim(my_msort9,[list(int),list(int)]).
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
p(['l','K','I','O','Y','j','I','K','r'],'I').
p(['C','y','Y','i','i','W','V','W','H','Z'],'W').
p(['p','D','f','f','V','R','M','Z'],'f').
p(['C','c','C','F','b','P','b','B','H'],'C').
p(['P','u','D','D','c','Y','U','T','F'],'D').
q(['O','p','H','w','T','O','N','T','e','K','q'],'w').
q(['x','W','Z','J','Q','C','C','u','x','H'],'u').
q(['w','V','v','z','Z','V','h','b','H'],'H').
q(['k','f','q','Z','X','s','I','C','C'],'I').
q(['t','m','I','e','R','x','t'],'x').
