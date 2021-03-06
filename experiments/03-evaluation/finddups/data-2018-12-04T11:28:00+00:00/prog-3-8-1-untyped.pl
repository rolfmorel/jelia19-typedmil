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
my_succ3(A,B):-succ(A,B),B =< 10.
my_flatten4(A,B):-flatten(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
my_sumlist6(A,B):-sumlist(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
my_msort8(A,B):-msort(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_pred10(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_succ3/2).
prim(my_flatten4/2).
prim(my_tolower5/2).
prim(my_sumlist6/2).
prim(my_double7/2).
prim(my_msort8/2).
prim(my_lowercase9/1).
prim(my_pred10/2).
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
p(['H','b','u','J','b','f','h','b','b'],'b').
p(['M','u','G','g','T','M','j'],'M').
p(['Q','Z','T','x','Z','Y'],'Z').
p(['o','b','Q','b','s','N','b','j','n','a'],'b').
p(['g','U','C','V','S','C','R','h','l','E'],'C').
q(['C','J','J','J','K','l','g'],'g').
q(['p','D','N','s','N','i'],'p').
q(['H','B','h','L','p','M','E','B'],'L').
q(['n','c','H','f','B','M','f','Y','h'],'M').
q(['o','q','V','e','e','T','R','p'],'T').
