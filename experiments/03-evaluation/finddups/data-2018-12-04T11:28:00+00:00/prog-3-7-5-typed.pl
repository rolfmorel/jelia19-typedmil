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
my_reverse3(A,B):-reverse(A,B).
my_max_list4(A,B):-max_list(A,B).
my_len5(A,B):-length(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_odd7(A):-1 is A mod 2.
my_sumlist8(A,B):-sumlist(A,B).
my_last9(A,B):-last(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_reverse3,[list(T),list(T)]).
prim(my_max_list4,[list(int),int]).
prim(my_len5,[list(_),int]).
prim(my_uppercase6,[char]).
prim(my_odd7,[int]).
prim(my_sumlist8,[list(int),int]).
prim(my_last9,[list(T),T]).
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
p(['y','n','T','g','L','z','y','S'],'y').
p(['L','A','Q','r','r','A','J','E','O','Z'],'A').
p(['B','a','N','k','a','I'],'a').
p(['Y','E','H','D','U','L','v','E','C','c'],'E').
p(['q','h','q','e','t','N','h'],'h').
q(['[','R','R','g','V','v'],'[').
q(['X','L','l','[','w','o','r','T','w','F','y'],'[').
q(['m','W','n','s','l','j','l','d'],'s').
q(['b','Z','w','a','l','z','B','P','n','a','b'],'w').
q(['T','s','x','Q','C','I','l','x','k','y','P'],'I').
