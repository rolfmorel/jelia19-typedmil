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
my_tolower4(A,B):-downcase_atom(A,B).
my_reverse5(A,B):-reverse(A,B).
my_min_list6(A,B):-min_list(A,B).
my_odd7(A):-1 is A mod 2.
my_lowercase8(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_list_to_set3/2).
prim(my_tolower4/2).
prim(my_reverse5/2).
prim(my_min_list6/2).
prim(my_odd7/1).
prim(my_lowercase8/1).
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
p(['V','K','f','i','f','H','Y','p','J','f'],'f').
p(['i','N','i','o','s','Y','C'],'i').
p(['M','a','M','a','a','J','W','v','k','z'],'a').
p(['c','u','r','t','M','i','t','b','S'],'t').
p(['Y','e','L','Y','H','k'],'Y').
q(['X','H','P','l','X','a'],'l').
q(['T','O','Z','u','n','n','F'],'Z').
q(['V','A','s','A','o','M'],'M').
q(['v','t','q','t','t','d','T','U'],'U').
q(['f','s','M','v','R','B','k','d','k','G'],'B').
