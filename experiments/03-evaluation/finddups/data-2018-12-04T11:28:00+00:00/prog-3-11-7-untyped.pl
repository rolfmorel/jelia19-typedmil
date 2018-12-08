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
my_odd3(A):-1 is A mod 2.
my_sumlist4(A,B):-sumlist(A,B).
my_flatten5(A,B):-flatten(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_even7(A):-0 is A mod 2.
my_max_list8(A,B):-max_list(A,B).
my_tolower9(A,B):-downcase_atom(A,B).
my_msort10(A,B):-msort(A,B).
my_reverse11(A,B):-reverse(A,B).
my_double12(N,M):-M is 2*N,M =< 10.
my_set13(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_odd3/1).
prim(my_sumlist4/2).
prim(my_flatten5/2).
prim(my_uppercase6/1).
prim(my_even7/1).
prim(my_max_list8/2).
prim(my_tolower9/2).
prim(my_msort10/2).
prim(my_reverse11/2).
prim(my_double12/2).
prim(my_set13/1).
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
p(['g','H','q','e','S','H','h'],'H').
p(['Z','Z','W','L','l','K','t'],'Z').
p(['k','m','D','x','x','H','n','H','P'],'H').
p(['t','y','y','l','I','S','I'],'I').
p(['A','Y','E','q','E'],'E').
q(['y','J','f','y','O','c'],'c').
q(['H','B','B','L','e','l','A','B','u','E','f'],'l').
q(['h','Q','h','L','l','o','P','E','J','V'],'E').
q(['R','n','[','N','N','c','j','J','e','E'],'[').
q(['n','n','S','e','v','E','W','c'],'v').
