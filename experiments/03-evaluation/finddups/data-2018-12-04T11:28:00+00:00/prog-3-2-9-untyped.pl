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
my_pred3(A,B):-succ(B,A),A > 0.
my_list_to_set4(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_pred3/2).
prim(my_list_to_set4/2).
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
p(['V','T','T','T','P','P','q','t','K'],'T').
p(['Q','r','a','S','F','Q','T','Q'],'Q').
p(['L','f','L','u','w','R','B'],'L').
p(['C','i','R','t','z','k','X','V','c','k'],'k').
p(['c','c','k','Q','J','c','I','D','D'],'c').
q(['v','d','h','W','h','J','E'],'E').
q(['L','Q','o','q','e','o','o','y','V','M','B'],'L').
q(['o','j','a','O','Z','t','N','o'],'a').
q(['U','O','U','I','D','O','J','x','w','U','r'],'I').
q(['l','b','o','l','y','z'],'b').
