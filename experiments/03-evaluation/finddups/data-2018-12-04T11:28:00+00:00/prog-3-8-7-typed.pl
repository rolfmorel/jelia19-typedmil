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
my_uppercase4(A):-upcase_atom(A,A).
my_msort5(A,B):-msort(A,B).
my_odd6(A):-1 is A mod 2.
my_last7(A,B):-last(A,B).
my_max_list8(A,B):-max_list(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_len10(A,B):-length(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_reverse3,[list(T),list(T)]).
prim(my_uppercase4,[char]).
prim(my_msort5,[list(int),list(int)]).
prim(my_odd6,[int]).
prim(my_last7,[list(T),T]).
prim(my_max_list8,[list(int),int]).
prim(my_pred9,[int,int]).
prim(my_len10,[list(_),int]).
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
p(['y','m','g','v','m','s','P','j','m'],'m').
p(['b','m','l','m','r','H','k','n'],'m').
p(['p','C','D','l','L','D','Q','K'],'D').
p(['r','Q','B','R','r','Q','n','L','J'],'r').
p(['n','g','T','B','P','g','P','s'],'g').
q(['r','H','G','y','j','h','m','G'],'j').
q(['i','d','F','z','j','P','X','d','p','Q'],'i').
q(['X','T','e','T','H','O','H','h'],'h').
q(['w','x','c','w','w','s','z','j'],'x').
q(['f','k','L','M','V','k','q'],'M').
