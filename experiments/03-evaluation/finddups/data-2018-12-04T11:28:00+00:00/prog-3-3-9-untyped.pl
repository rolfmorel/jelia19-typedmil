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
my_odd3(A):-1 is A mod 2.
my_last4(A,B):-last(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_odd3/1).
prim(my_last4/2).
prim(my_toupper5/2).
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
p(['S','j','N','V','W','V'],'V').
p(['E','t','i','t','Y','h','B','I','J','Y'],'t').
p(['M','c','w','k','z','D','r','q','q'],'q').
p(['P','g','R','l','S','p','R','l'],'l').
p(['z','v','f','v','Z','o','r','r'],'r').
q(['s','q','q','b','C','C','e','p','Q'],'Q').
q(['U','j','C','J','i','p','P','D','r','C','S'],'D').
q(['X','m','B','U','p','X','Y'],'p').
q(['M','u','N','a','u','X'],'N').
q(['G','o','T','G','{','i','w'],'{').
