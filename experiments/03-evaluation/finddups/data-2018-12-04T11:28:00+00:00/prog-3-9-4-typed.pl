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
my_toupper3(A,B):-upcase_atom(A,B).
my_flatten4(A,B):-flatten(A,B).
my_min_list5(A,B):-min_list(A,B).
my_last6(A,B):-last(A,B).
my_max_list7(A,B):-max_list(A,B).
my_msort8(A,B):-msort(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_tolower10(A,B):-downcase_atom(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_toupper3,[char,char]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_min_list5,[list(int),int]).
prim(my_last6,[list(T),T]).
prim(my_max_list7,[list(int),int]).
prim(my_msort8,[list(int),list(int)]).
prim(my_sumlist9,[list(int),int]).
prim(my_tolower10,[char,char]).
prim(my_pred11,[int,int]).
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
p(['v','p','h','J','p','p','o','V'],'p').
p(['p','Z','j','u','j'],'j').
p(['I','R','I','Z','C','T','U','a'],'I').
p(['A','O','z','A','Z','d','j'],'A').
p(['v','m','T','T','Y','G','u','H','h','g'],'T').
q(['H','x','r','q','d','q','p','x'],'H').
q(['K','y','w','y','H','p','G','v','U','N'],'K').
q(['v','W','O','t','L','y','v'],'L').
q(['S','U','d','u','V','Q','R','u','X','s','Y'],'U').
q(['Z','J','a','U','d','M','I','Y','U','U'],'a').
