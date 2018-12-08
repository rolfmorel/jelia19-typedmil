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
my_reverse3(A,B):-reverse(A,B).
my_toupper4(A,B):-upcase_atom(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_uppercase7(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_toupper4/2).
prim(my_sumlist5/2).
prim(my_succ6/2).
prim(my_uppercase7/1).
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
p(['e','c','z','j','C','c','k','n'],'c').
p(['K','K','P','X','u','f','o'],'K').
p(['A','L','a','S','a','T','Z'],'a').
p(['g','G','F','w','R','F','X','r','Q','z'],'F').
p(['Y','R','q','x','x','g'],'x').
q(['d','L','c','l','l','K','J','a','B','s','K'],'c').
q(['Q','C','d','o','Y','v','Q'],'Y').
q(['L','j','a','N','u','r','A','u','B','y','s'],'L').
q(['b','h','T','h','E','B','f','Y','O','B','y'],'O').
q(['u','l','O','l','F','p'],'p').
