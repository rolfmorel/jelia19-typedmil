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
my_min_list3(A,B):-min_list(A,B).
my_last4(A,B):-last(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_set6(A):-list_to_set(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_flatten8(A,B):-flatten(A,B).
my_max_list9(A,B):-max_list(A,B).
my_odd10(A):-1 is A mod 2.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_min_list3,[list(int),int]).
prim(my_last4,[list(T),T]).
prim(my_succ5,[int,int]).
prim(my_set6,[list(_)]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_max_list9,[list(int),int]).
prim(my_odd10,[int]).
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
p(['f','R','D','g','f','W','R','D','g','k'],'f').
p(['i','t','M','J','C','v','u','C','D'],'C').
p(['X','a','a','t','D','E','t'],'a').
p(['n','I','n','O','M'],'n').
p(['n','U','z','b','b','O','U','r'],'b').
q(['A','s','u','A','i','V','P'],'s').
q(['w','H','Z','t','f','j','Z','p','C','z','b'],'j').
q(['X','D','E','e','L','u','R','u','A','x'],'D').
q(['I','Z','e','q','g','Y','e','t','S','g'],'Y').
q(['n','T','e','E','X','G','P','M','X'],'e').
