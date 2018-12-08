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
my_even3(A):-0 is A mod 2.
my_min_list4(A,B):-min_list(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_max_list7(A,B):-max_list(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_min_list4/2).
prim(my_sumlist5/2).
prim(my_list_to_set6/2).
prim(my_max_list7/2).
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
p(['K','j','d','y','x','g','t','k','d'],'d').
p(['t','e','E','T','X','t','b'],'t').
p(['C','x','i','m','C','n','Y'],'C').
p(['i','V','n','Z','i','I','b'],'i').
p(['g','t','w','R','a','a','a','B'],'a').
q(['x','x','i','D','H','M'],'H').
q(['g','P','E','H','H','Q','K','G'],'P').
q(['q','L','e','u','S','h','y','L','w'],'y').
q(['a','y','Y','a','D','Z','P'],'Y').
q(['W','C','k','l','{','e','L','W','I'],'{').
