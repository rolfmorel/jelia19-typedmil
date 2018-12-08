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
my_len3(A,B):-length(A,B).
my_msort4(A,B):-msort(A,B).
my_uppercase5(A):-upcase_atom(A,A).
my_reverse6(A,B):-reverse(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_sumlist8(A,B):-sumlist(A,B).
my_odd9(A):-1 is A mod 2.
my_last10(A,B):-last(A,B).
my_lowercase11(A):-downcase_atom(A,A).
my_min_list12(A,B):-min_list(A,B).
my_even13(A):-0 is A mod 2.
my_toupper14(A,B):-upcase_atom(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
my_succ16(A,B):-succ(A,B),B =< 10.
my_flatten17(A,B):-flatten(A,B).
my_max_list18(A,B):-max_list(A,B).
my_list_to_set19(A,B):-list_to_set(A,B).
my_set20(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_msort4/2).
prim(my_uppercase5/1).
prim(my_reverse6/2).
prim(my_pred7/2).
prim(my_sumlist8/2).
prim(my_odd9/1).
prim(my_last10/2).
prim(my_lowercase11/1).
prim(my_min_list12/2).
prim(my_even13/1).
prim(my_toupper14/2).
prim(my_double15/2).
prim(my_succ16/2).
prim(my_flatten17/2).
prim(my_max_list18/2).
prim(my_list_to_set19/2).
prim(my_set20/1).
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
p(['Y','t','L','L','O','A','I','q','t'],'t').
p(['J','D','J','a','U','y','t','N'],'J').
p(['J','P','N','C','d','u','P','V','C'],'P').
p(['Q','W','o','W','M','p','F'],'W').
p(['w','z','Z','c','n','o','X','G','G','L'],'G').
q(['P','z','g','E','s','z','K','B','o'],'g').
q(['j','T','O','R','s','e','z','Z','p','X','p'],'Z').
q(['K','M','x','B','M','S','i'],'x').
q(['N','n','Z','m','i','k','l','E','s','n','U'],'m').
q(['C','U','b','e','d','m','d','L','C','J'],'m').
