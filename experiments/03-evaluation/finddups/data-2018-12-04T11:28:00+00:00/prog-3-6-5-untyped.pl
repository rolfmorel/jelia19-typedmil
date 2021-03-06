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
my_last5(A,B):-last(A,B).
my_min_list6(A,B):-min_list(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_toupper4/2).
prim(my_last5/2).
prim(my_min_list6/2).
prim(my_list_to_set7/2).
prim(my_tolower8/2).
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
p(['Y','J','l','n','J','N','I'],'J').
p(['b','q','j','b','d','Q','l'],'b').
p(['X','i','p','D','i','B'],'i').
p(['Y','M','b','M','N','R','W'],'M').
p(['Z','Z','L','a','Y'],'Z').
q(['W','r','X','q','f','i','K','I','K'],'q').
q(['R','F','m','T','J','J','o','g'],'T').
q(['l','o','p','C','h','z','R','s','h','c','r'],'p').
q(['n','S','S','m','w','c','u','d'],'c').
q(['O','g','u','o','o','l','e','t','X','u'],'e').
