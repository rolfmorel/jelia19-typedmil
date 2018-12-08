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
my_reverse4(A,B):-reverse(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_tolower7(A,B):-downcase_atom(A,B).
my_pred8(A,B):-succ(B,A),A > 0.
my_succ9(A,B):-succ(A,B),B =< 10.
my_min_list10(A,B):-min_list(A,B).
my_msort11(A,B):-msort(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_reverse4/2).
prim(my_toupper5/2).
prim(my_uppercase6/1).
prim(my_tolower7/2).
prim(my_pred8/2).
prim(my_succ9/2).
prim(my_min_list10/2).
prim(my_msort11/2).
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
p(['R','E','i','c','R'],'R').
p(['Q','t','X','F','C','E','F','Q'],'F').
p(['V','t','X','i','B','C','A','i'],'i').
p(['k','i','C','S','C'],'C').
p(['v','J','J','a','S'],'J').
q(['R','F','c','v','J','J'],'F').
q(['J','B','L','g','z','e','G','P','b','E','b'],'e').
q(['f','e','f','P','g','q','g'],'q').
q(['g','z','V','S','E','l','g','n','R'],'R').
q(['i','T','G','G','N','r','M','m'],'T').
