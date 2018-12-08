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
my_sumlist3(A,B):-sumlist(A,B).
my_reverse4(A,B):-reverse(A,B).
my_odd5(A):-1 is A mod 2.
my_uppercase6(A):-upcase_atom(A,A).
my_list_to_set7(A,B):-list_to_set(A,B).
my_last8(A,B):-last(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_sumlist3/2).
prim(my_reverse4/2).
prim(my_odd5/1).
prim(my_uppercase6/1).
prim(my_list_to_set7/2).
prim(my_last8/2).
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
p(['Z','L','o','i','z','p','i','O','L'],'i').
p(['u','B','e','B','Y','o','A','H','q'],'B').
p(['y','T','W','N','T','t','B','A'],'T').
p(['w','p','w','P','O','Y','F','P'],'w').
p(['Y','u','f','f','T','i','b'],'f').
q(['p','O','p','s','i','Y'],'O').
q(['m','s','H','r','s','i'],'m').
q(['o','h','t','Q','p','U','T','L','d','f','h'],'T').
q(['p','H','m','y','k','k','F','j','i','i'],'m').
q(['F','k','D','r','z','S','n','F'],'n').