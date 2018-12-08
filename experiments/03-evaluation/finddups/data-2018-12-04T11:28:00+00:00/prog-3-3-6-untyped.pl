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
my_list_to_set3(A,B):-list_to_set(A,B).
my_min_list4(A,B):-min_list(A,B).
my_even5(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_list_to_set3/2).
prim(my_min_list4/2).
prim(my_even5/1).
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
p(['Q','i','m','n','r','X','B','i','g','T'],'i').
p(['Z','O','y','y','G','a'],'y').
p(['Q','R','i','A','x','S','Q','R','f','t'],'Q').
p(['b','y','M','o','o'],'o').
p(['W','W','V','y','w'],'W').
q(['J','J','c','R','h','U'],'R').
q(['i','t','n','z','m','l','C','C','h'],'i').
q(['Z','V','G','Y','N','G'],'V').
q(['x','Q','r','w','l','r','a'],'a').
q(['O','g','v','y','O','f','Q','N','v','K'],'g').
