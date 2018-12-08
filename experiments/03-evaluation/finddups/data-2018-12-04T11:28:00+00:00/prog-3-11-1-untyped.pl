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
my_min_list3(A,B):-min_list(A,B).
my_msort4(A,B):-msort(A,B).
my_toupper5(A,B):-upcase_atom(A,B).
my_flatten6(A,B):-flatten(A,B).
my_lowercase7(A):-downcase_atom(A,A).
my_even8(A):-0 is A mod 2.
my_last9(A,B):-last(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_sumlist11(A,B):-sumlist(A,B).
my_pred12(A,B):-succ(B,A),A > 0.
my_len13(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_msort4/2).
prim(my_toupper5/2).
prim(my_flatten6/2).
prim(my_lowercase7/1).
prim(my_even8/1).
prim(my_last9/2).
prim(my_double10/2).
prim(my_sumlist11/2).
prim(my_pred12/2).
prim(my_len13/2).
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
p(['b','K','d','V','s','o','o','N','U'],'o').
p(['t','P','u','D','k','B','B'],'B').
p(['y','u','h','u','u','n','V'],'u').
p(['d','z','Z','z','V','n','z'],'z').
p(['T','c','U','f','a','S','A','J','J','U'],'U').
q(['[','q','o','M','i','o'],'[').
q(['Z','i','q','X','w','X'],'q').
q(['j','H','Q','S','R','q','H','t'],'R').
q(['f','O','Q','M','b','U','D','Y','Y'],'Q').
q(['o','f','F','X','D','L','J','F'],'f').
