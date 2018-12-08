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
my_odd4(A):-1 is A mod 2.
my_msort5(A,B):-msort(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_last7(A,B):-last(A,B).
my_flatten8(A,B):-flatten(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_len3,[list(_),int]).
prim(my_odd4,[int]).
prim(my_msort5,[list(int),list(int)]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_last7,[list(T),T]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_pred9,[int,int]).
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
p(['d','X','N','X','h','d','U'],'d').
p(['m','u','r','u','a'],'u').
p(['l','P','R','R','H','R'],'R').
p(['A','s','I','F','g','F','i'],'F').
p(['E','D','t','R','S','D','Q','O'],'D').
q(['B','U','c','L','g','I','I','z','i','h','M'],'g').
q(['m','A','N','s','A','n','r','s','s','Q','l'],'m').
q(['U','X','L','S','V','Z','U','T'],'L').
q(['J','u','d','O','O','x','v','R','s','x','W'],'R').
q(['k','D','O','i','i','P','P'],'D').
