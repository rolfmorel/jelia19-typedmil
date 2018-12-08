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
my_toupper4(A,B):-upcase_atom(A,B).
my_min_list5(A,B):-min_list(A,B).
my_last6(A,B):-last(A,B).
my_odd7(A):-1 is A mod 2.
my_max_list8(A,B):-max_list(A,B).
my_len9(A,B):-length(A,B).
my_flatten10(A,B):-flatten(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_toupper4,[char,char]).
prim(my_min_list5,[list(int),int]).
prim(my_last6,[list(T),T]).
prim(my_odd7,[int]).
prim(my_max_list8,[list(int),int]).
prim(my_len9,[list(_),int]).
prim(my_flatten10,[list(list(T)),list(T)]).
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
p(['a','m','l','h','P','P'],'P').
p(['o','u','x','N','N','g','a','G'],'N').
p(['F','g','L','m','e','L'],'L').
p(['w','T','i','B','v','Y','U','v'],'v').
p(['f','O','d','j','d','F','d','U','K'],'d').
q(['B','E','V','I','v','D','f','a','D','o','B'],'v').
q(['e','A','h','U','A','q','K'],'K').
q(['u','G','G','c','D','k','u','X','f','s','I'],'k').
q(['s','r','V','D','G','C','I','b','g','V'],'D').
q(['u','w','Z','y','D','l','b','w','C','s','L'],'s').
