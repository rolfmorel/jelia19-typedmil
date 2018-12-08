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
my_set3(A):-list_to_set(A,A).
my_pred4(A,B):-succ(B,A),A > 0.
my_min_list5(A,B):-min_list(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_uppercase7(A):-upcase_atom(A,A).
my_last8(A,B):-last(A,B).
my_reverse9(A,B):-reverse(A,B).
my_odd10(A):-1 is A mod 2.
my_max_list11(A,B):-max_list(A,B).
my_even12(A):-0 is A mod 2.
my_toupper13(A,B):-upcase_atom(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_lowercase15(A):-downcase_atom(A,A).
my_flatten16(A,B):-flatten(A,B).
my_double17(N,M):-M is 2*N,M =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_set3/1).
prim(my_pred4/2).
prim(my_min_list5/2).
prim(my_succ6/2).
prim(my_uppercase7/1).
prim(my_last8/2).
prim(my_reverse9/2).
prim(my_odd10/1).
prim(my_max_list11/2).
prim(my_even12/1).
prim(my_toupper13/2).
prim(my_sumlist14/2).
prim(my_lowercase15/1).
prim(my_flatten16/2).
prim(my_double17/2).
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
p(['a','c','a','o','M'],'a').
p(['m','z','m','M','s','Y'],'m').
p(['y','g','N','C','y','m','X','E'],'y').
p(['w','i','d','F','R','d'],'d').
p(['N','U','R','R','a','c','Y','N','Z'],'N').
q(['H','w','J','x','Q','L','V','n','J'],'w').
q(['L','E','A','m','h','h','T','W'],'E').
q(['z','f','o','o','t','c'],'c').
q(['F','s','P','P','h','s','k','Q','F'],'k').
q(['K','o','G','K','o','r','q','P','L'],'G').
