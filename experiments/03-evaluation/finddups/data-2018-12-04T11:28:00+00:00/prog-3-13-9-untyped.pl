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
my_last3(A,B):-last(A,B).
my_msort4(A,B):-msort(A,B).
my_min_list5(A,B):-min_list(A,B).
my_max_list6(A,B):-max_list(A,B).
my_tolower7(A,B):-downcase_atom(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_double9(N,M):-M is 2*N,M =< 10.
my_len10(A,B):-length(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_odd12(A):-1 is A mod 2.
my_succ13(A,B):-succ(A,B),B =< 10.
my_lowercase14(A):-downcase_atom(A,A).
my_even15(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_last3/2).
prim(my_msort4/2).
prim(my_min_list5/2).
prim(my_max_list6/2).
prim(my_tolower7/2).
prim(my_uppercase8/1).
prim(my_double9/2).
prim(my_len10/2).
prim(my_sumlist11/2).
prim(my_odd12/1).
prim(my_succ13/2).
prim(my_lowercase14/1).
prim(my_even15/1).
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
p(['u','e','b','W','v','b','E','U'],'b').
p(['s','L','C','e','D','L','m','m','O'],'L').
p(['b','U','F','q','C','m','T','b','V'],'b').
p(['b','n','n','X','r','q','f','F','B','f'],'f').
p(['E','r','F','H','l','G','E','V'],'E').
q(['a','B','o','V','F','N','h','a','B','J','F'],'V').
q(['U','C','K','l','S','C'],'K').
q(['p','P','p','K','n','Q'],'n').
q(['d','p','O','Y','g','p','M','I'],'M').
q(['m','p','f','Y','u','U','i','m'],'f').
