:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
%metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_tail0([_|TL],TL).
my_head1([H|_],H).
my_element2(A,B):-member(B,A).
my_lowercase3(A):-downcase_atom(A,A).
my_min_list4(A,B):-min_list(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_len6(A,B):-length(A,B).
my_sumlist7(A,B):-sumlist(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_odd9(A):-1 is A mod 2.
my_succ10(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_lowercase3/1).
prim(my_min_list4/2).
prim(my_tolower5/2).
prim(my_len6/2).
prim(my_sumlist7/2).
prim(my_uppercase8/1).
prim(my_odd9/1).
prim(my_succ10/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p(['g','R','O','X','R','i','R','o'],'R').
p(['f','W','M','O','n','n','Q'],'n').
p(['e','X','V','m','l','X','K','g'],'X').
p(['e','I','K','I','f','u','X','f','R'],'I').
p(['h','D','h','e','y','A','i','Q'],'h').
q(['n','E','E','Q','R','O','o'],'R').
q(['B','R','K','N','u','u','C','Y'],'B').
q(['p','U','V','f','U','Y','T','H','W'],'V').
q(['X','k','k','C','C','P'],'X').
q(['S','u','C','K','r','C','P'],'P').
