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
my_list_to_set3(A,B):-list_to_set(A,B).
my_set4(A):-list_to_set(A,A).
my_odd5(A):-1 is A mod 2.
my_tolower6(A,B):-downcase_atom(A,B).
my_reverse7(A,B):-reverse(A,B).
my_msort8(A,B):-msort(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_even10(A):-0 is A mod 2.
my_double11(N,M):-M is 2*N,M =< 10.
my_last12(A,B):-last(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_succ14(A,B):-succ(A,B),B =< 10.
my_max_list15(A,B):-max_list(A,B).
my_len16(A,B):-length(A,B).
my_sumlist17(A,B):-sumlist(A,B).
my_pred18(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_list_to_set3/2).
prim(my_set4/1).
prim(my_odd5/1).
prim(my_tolower6/2).
prim(my_reverse7/2).
prim(my_msort8/2).
prim(my_lowercase9/1).
prim(my_even10/1).
prim(my_double11/2).
prim(my_last12/2).
prim(my_toupper13/2).
prim(my_succ14/2).
prim(my_max_list15/2).
prim(my_len16/2).
prim(my_sumlist17/2).
prim(my_pred18/2).
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
p(['G','B','u','o','n','s','n','s'],'n').
p(['X','g','T','K','k','c','K','k','K','o'],'K').
p(['n','X','s','n','N','P','v'],'n').
p(['t','l','Z','J','J','X','U','S','l'],'J').
p(['a','b','y','A','c','h','L','b','k','b'],'b').
q(['t','i','y','j','n','H','h','Q','s','H'],'t').
q(['e','s','I','s','D','G'],'e').
q(['N','W','V','X','A','t','L','X','x','X'],'t').
q(['T','l','b','a','I','D','a','h','D','F','a'],'F').
q(['W','R','W','j','R','G'],'G').
