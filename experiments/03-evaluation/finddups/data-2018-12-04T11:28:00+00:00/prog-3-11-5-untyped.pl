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
my_max_list4(A,B):-max_list(A,B).
my_flatten5(A,B):-flatten(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_succ7(A,B):-succ(A,B),B =< 10.
my_even8(A):-0 is A mod 2.
my_set9(A):-list_to_set(A,A).
my_msort10(A,B):-msort(A,B).
my_tolower11(A,B):-downcase_atom(A,B).
my_uppercase12(A):-upcase_atom(A,A).
my_sumlist13(A,B):-sumlist(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_max_list4/2).
prim(my_flatten5/2).
prim(my_pred6/2).
prim(my_succ7/2).
prim(my_even8/1).
prim(my_set9/1).
prim(my_msort10/2).
prim(my_tolower11/2).
prim(my_uppercase12/1).
prim(my_sumlist13/2).
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
p(['S','Q','v','I','F','v','X'],'v').
p(['h','R','R','r','o','u','T','o','i'],'o').
p(['H','X','d','K','T','b','q','Q','t','T'],'T').
p(['s','u','P','N','h','C','S','S','F','e'],'S').
p(['Q','d','d','J','b','e'],'d').
q(['c','o','X','I','m','E','E','V','Y'],'I').
q(['v','R','b','F','E','z','E'],'z').
q(['G','o','p','S','K','E','b','M','t','r','K'],'t').
q(['L','e','Z','Z','m','o','Y'],'L').
q(['D','s','z','Q','Q','b'],'s').
