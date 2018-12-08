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
my_flatten3(A,B):-flatten(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_set5(A):-list_to_set(A,A).
my_min_list6(A,B):-min_list(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_flatten3,[list(list(T)),list(T)]).
prim(my_lowercase4,[char]).
prim(my_set5,[list(_)]).
prim(my_min_list6,[list(int),int]).
prim(my_pred7,[int,int]).
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
p(['D','s','F','o','H','n','g','H','s','N'],'s').
p(['D','W','J','J','z','a','t','S','a'],'a').
p(['B','f','w','f','Z'],'f').
p(['l','f','n','R','l'],'l').
p(['v','D','T','j','j'],'j').
q(['k','W','q','W','r','T','U','O','Q'],'O').
q(['C','k','u','r','X','I','U','G','k'],'u').
q(['V','L','V','C','h','O','S','R'],'C').
q(['l','Z','Z','S','b','X','G','u','H','P'],'l').
q(['f','S','f','K','p','G','d','y','R','g','f'],'R').
