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
my_set3(A):-list_to_set(A,A).
my_len4(A,B):-length(A,B).
my_min_list5(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_set3/1).
prim(my_len4/2).
prim(my_min_list5/2).
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
p(['Y','W','a','a','u','m','N','p','M','g'],'a').
p(['u','E','P','P','m','m','y'],'P').
p(['n','P','C','i','G','n','c','q'],'n').
p(['v','H','i','g','P','P','o','i'],'P').
p(['s','S','T','a','X','a','b'],'a').
q(['i','Y','m','o','m','m','T','X','m','K','z'],'X').
q(['l','g','u','i','r','s','z','y','V','l','s'],'u').
q(['F','P','n','Z','n','q'],'F').
q(['h','i','S','D','y','w','i'],'y').
q(['W','x','K','D','k','T','D','R','p','z','F'],'K').
