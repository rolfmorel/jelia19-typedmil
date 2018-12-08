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
my_double3(N,M):-M is 2*N,M =< 10.
my_list_to_set4(A,B):-list_to_set(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_len6(A,B):-length(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
my_min_list8(A,B):-min_list(A,B).
my_max_list9(A,B):-max_list(A,B).
my_last10(A,B):-last(A,B).
my_set11(A):-list_to_set(A,A).
my_lowercase12(A):-downcase_atom(A,A).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_double3,[int,int]).
prim(my_list_to_set4,[list(T),list(T)]).
prim(my_sumlist5,[list(int),int]).
prim(my_len6,[list(_),int]).
prim(my_tolower7,[char,char]).
prim(my_min_list8,[list(int),int]).
prim(my_max_list9,[list(int),int]).
prim(my_last10,[list(T),T]).
prim(my_set11,[list(_)]).
prim(my_lowercase12,[char]).
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
p(['B','M','M','y','o','z','Z'],'M').
p(['W','g','v','q','Q','g','g','u'],'g').
p(['R','t','A','R','Z','w'],'R').
p(['b','D','b','h','m','h'],'b').
p(['A','x','A','F','C','r','h'],'A').
q(['i','W','g','u','K','a','i','J','a','p'],'K').
q(['u','r','j','i','w','w','Y','R'],'i').
q(['i','a','h','b','B','g','a','A','a','i','N'],'B').
q(['A','F','e','d','F','r','J','v','q'],'e').
q(['i','V','c','C','S','y','C','F','R','T'],'y').
