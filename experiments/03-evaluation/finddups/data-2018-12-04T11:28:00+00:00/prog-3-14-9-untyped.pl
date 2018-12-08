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
my_succ3(A,B):-succ(A,B),B =< 10.
my_min_list4(A,B):-min_list(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_max_list7(A,B):-max_list(A,B).
my_tolower8(A,B):-downcase_atom(A,B).
my_msort9(A,B):-msort(A,B).
my_odd10(A):-1 is A mod 2.
my_set11(A):-list_to_set(A,A).
my_double12(N,M):-M is 2*N,M =< 10.
my_uppercase13(A):-upcase_atom(A,A).
my_reverse14(A,B):-reverse(A,B).
my_sumlist15(A,B):-sumlist(A,B).
my_lowercase16(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_succ3/2).
prim(my_min_list4/2).
prim(my_list_to_set5/2).
prim(my_pred6/2).
prim(my_max_list7/2).
prim(my_tolower8/2).
prim(my_msort9/2).
prim(my_odd10/1).
prim(my_set11/1).
prim(my_double12/2).
prim(my_uppercase13/1).
prim(my_reverse14/2).
prim(my_sumlist15/2).
prim(my_lowercase16/1).
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
p(['P','f','W','i','D','W','W'],'W').
p(['h','G','l','i','v','J','i'],'i').
p(['w','q','i','i','w','h','w','M'],'i').
p(['Q','N','f','u','N','s'],'N').
p(['I','O','k','s','x','g','k'],'k').
q(['P','U','p','U','N','U','R','Q'],'p').
q(['u','u','e','r','N','n','t','c'],'t').
q(['c','K','Y','t','q','z','Y','z'],'c').
q(['g','j','V','W','P','i','W'],'V').
q(['I','h','o','R','u','T','T'],'u').
