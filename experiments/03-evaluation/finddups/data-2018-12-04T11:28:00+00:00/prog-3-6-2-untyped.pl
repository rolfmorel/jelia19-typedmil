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
my_reverse5(A,B):-reverse(A,B).
my_tolower6(A,B):-downcase_atom(A,B).
my_msort7(A,B):-msort(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_last4/2).
prim(my_reverse5/2).
prim(my_tolower6/2).
prim(my_msort7/2).
prim(my_toupper8/2).
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
p(['f','y','y','n','w','i','o'],'y').
p(['l','p','D','u','w','l'],'l').
p(['O','g','q','q','Y','d','B'],'q').
p(['b','Z','x','A','X','b','o'],'b').
p(['O','w','w','x','I'],'w').
q(['r','j','D','K','N','O','r'],'O').
q(['c','G','R','A','Y','F','J','A'],'G').
q(['W','L','B','g','S','s','x','g','H'],'L').
q(['l','R','S','W','W','p','o','X','a','K'],'p').
q(['q','Z','A','F','n','F'],'Z').
