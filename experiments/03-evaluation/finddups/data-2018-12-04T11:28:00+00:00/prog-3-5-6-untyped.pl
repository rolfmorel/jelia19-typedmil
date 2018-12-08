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
my_toupper3(A,B):-upcase_atom(A,B).
my_lowercase4(A):-downcase_atom(A,A).
my_sumlist5(A,B):-sumlist(A,B).
my_even6(A):-0 is A mod 2.
my_len7(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_toupper3/2).
prim(my_lowercase4/1).
prim(my_sumlist5/2).
prim(my_even6/1).
prim(my_len7/2).
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
p(['x','I','X','U','b','E','X','j','E'],'E').
p(['d','Z','Q','b','f','C','y','F','y','O'],'y').
p(['E','w','J','H','H','Q','M'],'H').
p(['H','S','R','g','G','g','D','E','s'],'g').
p(['j','p','T','z','c','p'],'p').
q(['q','A','U','F','A','t','O'],'F').
q(['V','C','T','i','F','i','K','C'],'K').
q(['R','o','Y','q','v','G','m','i','M','p','q'],'v').
q(['t','j','r','O','h','k','t','O','H','u','l'],'j').
q(['y','o','e','x','v','C','X','f','j','x'],'o').
