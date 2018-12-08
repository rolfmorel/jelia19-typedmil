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
my_sumlist3(A,B):-sumlist(A,B).
my_flatten4(A,B):-flatten(A,B).
my_min_list5(A,B):-min_list(A,B).
my_max_list6(A,B):-max_list(A,B).
my_even7(A):-0 is A mod 2.
my_set8(A):-list_to_set(A,A).
my_pred9(A,B):-succ(B,A),A > 0.
my_reverse10(A,B):-reverse(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_sumlist3,[list(int),int]).
prim(my_flatten4,[list(list(T)),list(T)]).
prim(my_min_list5,[list(int),int]).
prim(my_max_list6,[list(int),int]).
prim(my_even7,[int]).
prim(my_set8,[list(_)]).
prim(my_pred9,[int,int]).
prim(my_reverse10,[list(T),list(T)]).
prim(my_list_to_set11,[list(T),list(T)]).
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
p(['h','j','E','H','h','z','S','g','g','N'],'h').
p(['T','v','m','r','v','t','J','B','U','e'],'v').
p(['n','W','R','Z','O','n','R'],'n').
p(['H','M','o','Q','M','J','f'],'M').
p(['X','v','q','o','w','v','O'],'v').
q(['w','a','K','B','u','o','w','p'],'a').
q(['J','y','r','I','P','Q','D','J','N','k'],'y').
q(['G','D','A','Q','Q','Q','y'],'D').
q(['A','A','w','S','h','r'],'h').
q(['s','C','t','r','o','s','E','X','K','l'],'r').
