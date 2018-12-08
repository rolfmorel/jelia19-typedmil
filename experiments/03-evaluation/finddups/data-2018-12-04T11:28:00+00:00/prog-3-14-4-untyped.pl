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
my_last4(A,B):-last(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_set6(A):-list_to_set(A,A).
my_odd7(A):-1 is A mod 2.
my_uppercase8(A):-upcase_atom(A,A).
my_msort9(A,B):-msort(A,B).
my_reverse10(A,B):-reverse(A,B).
my_double11(N,M):-M is 2*N,M =< 10.
my_sumlist12(A,B):-sumlist(A,B).
my_len13(A,B):-length(A,B).
my_tolower14(A,B):-downcase_atom(A,B).
my_list_to_set15(A,B):-list_to_set(A,B).
my_even16(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_max_list3/2).
prim(my_last4/2).
prim(my_succ5/2).
prim(my_set6/1).
prim(my_odd7/1).
prim(my_uppercase8/1).
prim(my_msort9/2).
prim(my_reverse10/2).
prim(my_double11/2).
prim(my_sumlist12/2).
prim(my_len13/2).
prim(my_tolower14/2).
prim(my_list_to_set15/2).
prim(my_even16/1).
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
p(['r','c','f','n','j','X','n'],'n').
p(['t','M','t','t','M','R'],'t').
p(['N','u','R','C','o','R'],'R').
p(['A','f','A','L','R','N'],'A').
p(['I','E','V','H','I'],'I').
q(['o','l','O','j','w','Z','N','x','W','w'],'N').
q(['G','c','q','W','k','o','b','g','C','r','r'],'C').
q(['j','y','w','X','f','a','y'],'a').
q(['P','O','d','Y','B','W','d','E','d'],'E').
q(['a','E','Y','P','W','r','b','p','p'],'P').
