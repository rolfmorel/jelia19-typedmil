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
my_sumlist4(A,B):-sumlist(A,B).
my_uppercase5(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_sumlist4/2).
prim(my_uppercase5/1).
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
p(['C','Z','s','q','F','q','E','s'],'q').
p(['x','U','H','U','i'],'U').
p(['h','X','q','B','h','j','B','a','O'],'B').
p(['G','v','B','R','U','M','U','p','R'],'R').
p(['A','k','o','R','n','q','B','B','X'],'B').
q(['B','z','G','D','T','U','i','s','T'],'z').
q(['R','Z','J','T','R','x','A'],'A').
q(['I','d','M','k','T','p','p','U','Z','M','x'],'U').
q(['X','A','A','R','s','p','o','W','i','u','n'],'R').
q(['j','x','L','i','q','T','j','O'],'x').
