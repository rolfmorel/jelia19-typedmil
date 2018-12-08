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
my_msort3(A,B):-msort(A,B).
my_tolower4(A,B):-downcase_atom(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_odd6(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_msort3/2).
prim(my_tolower4/2).
prim(my_list_to_set5/2).
prim(my_odd6/1).
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
p(['d','X','A','A','x','E','u','D','u','v'],'u').
p(['T','T','w','L','T','G','W','P','s'],'T').
p(['n','d','n','J','i','Z'],'n').
p(['E','O','C','T','z','z','Y'],'z').
p(['i','F','k','a','V','F'],'F').
q(['X','T','E','X','y','p','i','v','m','y','u'],'i').
q(['m','J','o','L','q','m'],'o').
q(['i','J','O','b','T','N','m','b','w','m','b'],'N').
q(['x','b','x','s','u','s','p','I'],'u').
q(['r','L','V','r','r','K','d','c','[','g'],'[').
