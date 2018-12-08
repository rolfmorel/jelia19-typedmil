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
my_min_list4(A,B):-min_list(A,B).
my_pred5(A,B):-succ(B,A),A > 0.
my_toupper6(A,B):-upcase_atom(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_reverse8(A,B):-reverse(A,B).
my_flatten9(A,B):-flatten(A,B).
my_max_list10(A,B):-max_list(A,B).
my_odd11(A):-1 is A mod 2.
my_len12(A,B):-length(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_last14(A,B):-last(A,B).
my_msort15(A,B):-msort(A,B).
my_lowercase16(A):-downcase_atom(A,A).
my_double17(N,M):-M is 2*N,M =< 10.
my_list_to_set18(A,B):-list_to_set(A,B).
my_tolower19(A,B):-downcase_atom(A,B).
my_sumlist20(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_min_list4/2).
prim(my_pred5/2).
prim(my_toupper6/2).
prim(my_succ7/2).
prim(my_reverse8/2).
prim(my_flatten9/2).
prim(my_max_list10/2).
prim(my_odd11/1).
prim(my_len12/2).
prim(my_uppercase13/1).
prim(my_last14/2).
prim(my_msort15/2).
prim(my_lowercase16/1).
prim(my_double17/2).
prim(my_list_to_set18/2).
prim(my_tolower19/2).
prim(my_sumlist20/2).
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
p(['c','h','W','M','W','C','A'],'W').
p(['O','k','Y','m','O','V','I','h'],'O').
p(['K','U','F','z','b','b','h','g'],'b').
p(['P','v','H','S','K','S'],'S').
p(['k','w','j','x','c','x','n','q','r','C'],'x').
q(['a','u','e','r','j','S','A','r'],'u').
q(['S','m','Q','O','m','f','E','P','Q'],'S').
q(['j','z','x','x','f','b','r','S','c','F','a'],'f').
q(['s','T','t','s','E','v'],'E').
q(['G','Z','d','R','T','K','R','I'],'K').
