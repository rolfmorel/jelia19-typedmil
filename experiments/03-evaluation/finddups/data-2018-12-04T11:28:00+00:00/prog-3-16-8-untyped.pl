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
my_toupper3(A,B):-upcase_atom(A,B).
my_succ4(A,B):-succ(A,B),B =< 10.
my_reverse5(A,B):-reverse(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_tolower7(A,B):-downcase_atom(A,B).
my_min_list8(A,B):-min_list(A,B).
my_odd9(A):-1 is A mod 2.
my_sumlist10(A,B):-sumlist(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_msort12(A,B):-msort(A,B).
my_even13(A):-0 is A mod 2.
my_len14(A,B):-length(A,B).
my_last15(A,B):-last(A,B).
my_set16(A):-list_to_set(A,A).
my_double17(N,M):-M is 2*N,M =< 10.
my_pred18(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_toupper3/2).
prim(my_succ4/2).
prim(my_reverse5/2).
prim(my_uppercase6/1).
prim(my_tolower7/2).
prim(my_min_list8/2).
prim(my_odd9/1).
prim(my_sumlist10/2).
prim(my_list_to_set11/2).
prim(my_msort12/2).
prim(my_even13/1).
prim(my_len14/2).
prim(my_last15/2).
prim(my_set16/1).
prim(my_double17/2).
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
p(['i','h','f','z','c','z','z','K','F','I'],'z').
p(['v','n','w','n','R','U','P'],'n').
p(['z','D','u','z','D','G'],'z').
p(['a','R','p','B','E','V','a','l','t','p'],'p').
p(['M','M','E','d','v','p','P'],'M').
q(['F','O','R','c','v','z','R','V','X','f'],'c').
q(['N','v','z','z','i','s'],'N').
q(['e','o','U','p','L','S','k','C','l','o'],'U').
q(['L','n','d','L','j','[','O'],'[').
q(['u','O','d','E','Z','Q','X','d','D','k','Y'],'Z').
