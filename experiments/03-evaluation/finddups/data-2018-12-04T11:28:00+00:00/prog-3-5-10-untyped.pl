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
my_flatten3(A,B):-flatten(A,B).
my_last4(A,B):-last(A,B).
my_odd5(A):-1 is A mod 2.
my_len6(A,B):-length(A,B).
my_double7(N,M):-M is 2*N,M =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_last4/2).
prim(my_odd5/1).
prim(my_len6/2).
prim(my_double7/2).
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
p(['v','p','w','A','v'],'v').
p(['o','c','D','s','o'],'o').
p(['J','s','q','a','J','E','J','f','Q'],'J').
p(['S','H','M','A','v','T','H','j','y'],'H').
p(['R','Y','l','r','y','S','r'],'r').
q(['K','v','Z','C','R','v','r','Z','s'],'K').
q(['J','H','q','A','h','h','I','y','a'],'I').
q(['B','N','b','b','H','o','X'],'o').
q(['J','D','M','G','h','Q','W','s','d','G','M'],'W').
q(['F','p','I','k','P','C','F','q'],'p').
