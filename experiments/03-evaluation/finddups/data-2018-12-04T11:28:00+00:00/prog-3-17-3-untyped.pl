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
my_lowercase4(A):-downcase_atom(A,A).
my_uppercase5(A):-upcase_atom(A,A).
my_sumlist6(A,B):-sumlist(A,B).
my_len7(A,B):-length(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_tolower9(A,B):-downcase_atom(A,B).
my_msort10(A,B):-msort(A,B).
my_last11(A,B):-last(A,B).
my_max_list12(A,B):-max_list(A,B).
my_odd13(A):-1 is A mod 2.
my_double14(N,M):-M is 2*N,M =< 10.
my_flatten15(A,B):-flatten(A,B).
my_toupper16(A,B):-upcase_atom(A,B).
my_min_list17(A,B):-min_list(A,B).
my_pred18(A,B):-succ(B,A),A > 0.
my_succ19(A,B):-succ(A,B),B =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_lowercase4/1).
prim(my_uppercase5/1).
prim(my_sumlist6/2).
prim(my_len7/2).
prim(my_list_to_set8/2).
prim(my_tolower9/2).
prim(my_msort10/2).
prim(my_last11/2).
prim(my_max_list12/2).
prim(my_odd13/1).
prim(my_double14/2).
prim(my_flatten15/2).
prim(my_toupper16/2).
prim(my_min_list17/2).
prim(my_pred18/2).
prim(my_succ19/2).
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
p(['x','q','S','a','M','l','t','l','d','U'],'l').
p(['E','i','h','w','w','L','h','h'],'h').
p(['Z','w','l','Y','B','W','B','w','g','O'],'B').
p(['n','B','N','v','v','J','m','B','B'],'v').
p(['f','Z','I','Z','Y'],'Z').
q(['d','C','F','Z','A','M','A','S'],'C').
q(['s','X','f','h','h','R'],'f').
q(['y','F','G','P','r','y','Z','r'],'F').
q(['M','a','M','P','e','u','Z','g','e','o'],'a').
q(['y','Q','d','K','y','D'],'d').
