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
my_flatten4(A,B):-flatten(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_flatten4/2).
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
p(['s','j','T','y','G','i','q','R','q'],'q').
p(['e','D','v','H','h','J','D'],'D').
p(['V','u','m','o','e','K','H','u'],'u').
p(['U','Q','T','T','Y'],'T').
p(['N','c','K','P','N','O'],'N').
q(['b','V','w','F','V','r','U','r'],'w').
q(['B','b','o','q','B','P','n','x','B','e'],'n').
q(['R','t','R','u','r','q'],'q').
q(['E','X','c','E','d','d'],'c').
q(['p','F','A','F','u','c','H','T','l','D'],'u').
