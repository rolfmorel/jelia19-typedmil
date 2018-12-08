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
my_tolower3(A,B):-downcase_atom(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.
my_odd5(A):-1 is A mod 2.
my_len6(A,B):-length(A,B).
my_max_list7(A,B):-max_list(A,B).
my_flatten8(A,B):-flatten(A,B).
my_even9(A):-0 is A mod 2.
my_set10(A):-list_to_set(A,A).
my_lowercase11(A):-downcase_atom(A,A).
my_reverse12(A,B):-reverse(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
prim(my_tail0,[list(T),list(T)]).
prim(my_head1,[list(T),T]).
prim(my_element2,[list(T),T]).
prim(my_tolower3,[char,char]).
prim(my_succ4,[int,int]).
prim(my_odd5,[int]).
prim(my_len6,[list(_),int]).
prim(my_max_list7,[list(int),int]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_even9,[int]).
prim(my_set10,[list(_)]).
prim(my_lowercase11,[char]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_list_to_set13,[list(T),list(T)]).
prim(my_toupper14,[char,char]).
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
p(['Z','a','N','l','F','z','N','V'],'N').
p(['V','Q','L','s','w','t','t','t'],'t').
p(['z','m','l','A','I','t','s','z','W'],'z').
p(['b','J','n','m','Q','U','t','Q'],'Q').
p(['D','N','u','D','S','a'],'D').
q(['a','m','J','o','C','C','N','C','X'],'a').
q(['D','x','p','d','H','d','u'],'x').
q(['V','Y','e','V','t','o','v','A'],'A').
q(['R','t','Y','W','I','j','I','x','S'],'x').
q(['I','r','y','O','i','t','I','A'],'A').
