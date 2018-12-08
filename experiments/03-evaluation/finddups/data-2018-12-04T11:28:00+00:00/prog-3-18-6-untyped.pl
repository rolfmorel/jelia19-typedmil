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
my_len4(A,B):-length(A,B).
my_lowercase5(A):-downcase_atom(A,A).
my_min_list6(A,B):-min_list(A,B).
my_uppercase7(A):-upcase_atom(A,A).
my_double8(N,M):-M is 2*N,M =< 10.
my_set9(A):-list_to_set(A,A).
my_tolower10(A,B):-downcase_atom(A,B).
my_toupper11(A,B):-upcase_atom(A,B).
my_reverse12(A,B):-reverse(A,B).
my_flatten13(A,B):-flatten(A,B).
my_sumlist14(A,B):-sumlist(A,B).
my_odd15(A):-1 is A mod 2.
my_list_to_set16(A,B):-list_to_set(A,B).
my_last17(A,B):-last(A,B).
my_max_list18(A,B):-max_list(A,B).
my_msort19(A,B):-msort(A,B).
my_pred20(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_len4/2).
prim(my_lowercase5/1).
prim(my_min_list6/2).
prim(my_uppercase7/1).
prim(my_double8/2).
prim(my_set9/1).
prim(my_tolower10/2).
prim(my_toupper11/2).
prim(my_reverse12/2).
prim(my_flatten13/2).
prim(my_sumlist14/2).
prim(my_odd15/1).
prim(my_list_to_set16/2).
prim(my_last17/2).
prim(my_max_list18/2).
prim(my_msort19/2).
prim(my_pred20/2).
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
p(['X','u','Y','I','Y','b','Y'],'Y').
p(['F','i','S','y','E','T','D','N','e','T'],'T').
p(['W','P','j','P','b','G','r','D'],'P').
p(['t','j','t','i','B','t','t'],'t').
p(['D','P','p','B','p','I','p','P','s'],'P').
q(['V','s','M','M','G','c'],'G').
q(['t','q','E','w','C','x','F','h','Q','E'],'Q').
q(['S','d','f','K','w','u','A','A','a'],'f').
q(['d','h','o','y','f','Z','f'],'d').
q(['H','H','j','H','m','U','F'],'m').
