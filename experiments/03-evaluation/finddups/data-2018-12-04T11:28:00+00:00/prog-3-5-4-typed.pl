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
my_list_to_set4(A,B):-list_to_set(A,B).
my_last5(A,B):-last(A,B).
my_even6(A):-0 is A mod 2.
my_set7(A):-list_to_set(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_pred3,[int,int]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_last5,[list(T),T]).
prim(my_even6,[int]).
prim(my_set7,[list(_)]).
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
p(['n','P','b','b','s','o','y'],'b').
p(['g','M','k','i','D','D'],'D').
p(['N','q','c','j','N','W'],'N').
p(['T','W','l','L','w','T','a'],'T').
p(['G','Z','Z','b','E','y'],'Z').
q(['y','p','r','X','r','K','q','Z','E'],'y').
q(['c','g','Y','R','g','C'],'c').
q(['b','I','p','W','l','Y','p','F'],'Y').
q(['h','W','B','n','u','u'],'n').
q(['W','w','W','r','W','R','p','O'],'O').
