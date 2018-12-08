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
my_lowercase4(A):-downcase_atom(A,A).
my_even5(A):-0 is A mod 2.
my_len6(A,B):-length(A,B).
my_last7(A,B):-last(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_list_to_set9(A,B):-list_to_set(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_reverse3,[list(T),list(T)]).
prim(my_lowercase4,[char]).
prim(my_even5,[int]).
prim(my_len6,[list(_),int]).
prim(my_last7,[list(T),T]).
prim(my_pred8,[int,int]).
prim(my_list_to_set9,[list(T),list(T)]).
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
p(['R','e','v','j','L','B','P','Y','L'],'L').
p(['f','m','W','G','W','z','C','D'],'W').
p(['l','b','l','l','Q'],'l').
p(['s','r','i','f','q','r','u'],'r').
p(['a','p','c','w','x','c','O','j','v','a'],'c').
q(['u','K','K','K','n','m','L','Z','F','u'],'L').
q(['V','T','H','o','A','J','n','J','f','x'],'V').
q(['F','V','n','F','u','I'],'u').
q(['X','w','f','n','P','Q','x','X','S','e','B'],'S').
q(['t','t','d','[','o','Y','b','s','f','u'],'[').
