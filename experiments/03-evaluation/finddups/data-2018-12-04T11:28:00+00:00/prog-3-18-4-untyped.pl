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
my_double4(N,M):-M is 2*N,M =< 10.
my_tolower5(A,B):-downcase_atom(A,B).
my_flatten6(A,B):-flatten(A,B).
my_len7(A,B):-length(A,B).
my_min_list8(A,B):-min_list(A,B).
my_msort9(A,B):-msort(A,B).
my_max_list10(A,B):-max_list(A,B).
my_last11(A,B):-last(A,B).
my_reverse12(A,B):-reverse(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_even14(A):-0 is A mod 2.
my_lowercase15(A):-downcase_atom(A,A).
my_pred16(A,B):-succ(B,A),A > 0.
my_succ17(A,B):-succ(A,B),B =< 10.
my_list_to_set18(A,B):-list_to_set(A,B).
my_sumlist19(A,B):-sumlist(A,B).
my_uppercase20(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_set3/1).
prim(my_double4/2).
prim(my_tolower5/2).
prim(my_flatten6/2).
prim(my_len7/2).
prim(my_min_list8/2).
prim(my_msort9/2).
prim(my_max_list10/2).
prim(my_last11/2).
prim(my_reverse12/2).
prim(my_toupper13/2).
prim(my_even14/1).
prim(my_lowercase15/1).
prim(my_pred16/2).
prim(my_succ17/2).
prim(my_list_to_set18/2).
prim(my_sumlist19/2).
prim(my_uppercase20/1).
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
p(['T','C','U','k','b','G','G','a','K','f'],'G').
p(['J','D','I','Z','J','d','J'],'J').
p(['H','U','h','U','w','d','w','S'],'U').
p(['C','X','V','E','O','P','X','u','b','s'],'X').
p(['W','q','w','j','K','w','P'],'w').
q(['Y','M','g','C','z','s','w','w'],'C').
q(['J','f','z','R','C','R','s','d','r','J'],'f').
q(['X','I','p','A','m','p'],'X').
q(['i','G','e','P','U','P','z','N','Q'],'i').
q(['s','j','Q','P','N','t','v','y','y'],'t').
