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
my_max_list3(A,B):-max_list(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_odd5(A):-1 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_max_list3/2).
prim(my_list_to_set4/2).
prim(my_odd5/1).
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
p(['R','j','Y','x','x','l'],'x').
p(['W','Y','e','X','N','T','e'],'e').
p(['D','s','B','A','D'],'D').
p(['X','O','L','u','z','i','Z','V','L','H'],'L').
p(['S','J','C','b','b'],'b').
q(['v','W','t','z','A','v','X','G','T'],'W').
q(['s','n','s','S','T','x','q','f','l','m'],'T').
q(['r','t','g','g','b','x','w','a','f'],'w').
q(['v','N','s','e','C','v','c','m','m'],'N').
q(['v','r','c','l','S','S','y','s','a'],'v').
