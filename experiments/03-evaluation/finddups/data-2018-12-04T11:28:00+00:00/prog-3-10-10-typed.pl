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
my_double4(N,M):-M is 2*N,M =< 10.
my_min_list5(A,B):-min_list(A,B).
my_set6(A):-list_to_set(A,A).
my_lowercase7(A):-downcase_atom(A,A).
my_tolower8(A,B):-downcase_atom(A,B).
my_len9(A,B):-length(A,B).
my_flatten10(A,B):-flatten(A,B).
my_even11(A):-0 is A mod 2.
my_succ12(A,B):-succ(A,B),B =< 10.
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_toupper3,[char,char]).
prim(my_double4,[int,int]).
prim(my_min_list5,[list(int),int]).
prim(my_set6,[list(_)]).
prim(my_lowercase7,[char]).
prim(my_tolower8,[char,char]).
prim(my_len9,[list(_),int]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_even11,[int]).
prim(my_succ12,[int,int]).
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
p(['z','t','g','g','H','S','s'],'g').
p(['e','j','z','X','z','A','M','W','Z','l'],'z').
p(['c','h','S','A','l','w','h','Z','U','U'],'h').
p(['D','c','m','w','d','K','K','U','d'],'K').
p(['v','i','T','Y','X','c','c'],'c').
q(['o','d','B','b','c','y','F','d','D'],'c').
q(['X','k','q','W','g','O','o','M','o','Q','t'],'t').
q(['X','W','h','A','t','p','A','d','l','k','l'],'k').
q(['a','A','G','v','A','i','w'],'a').
q(['K','H','K','c','Z','H','S','d'],'S').
