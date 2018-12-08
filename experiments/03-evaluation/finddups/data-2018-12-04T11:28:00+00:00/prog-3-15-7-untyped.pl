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
my_flatten3(A,B):-flatten(A,B).
my_msort4(A,B):-msort(A,B).
my_odd5(A):-1 is A mod 2.
my_set6(A):-list_to_set(A,A).
my_pred7(A,B):-succ(B,A),A > 0.
my_max_list8(A,B):-max_list(A,B).
my_list_to_set9(A,B):-list_to_set(A,B).
my_succ10(A,B):-succ(A,B),B =< 10.
my_double11(N,M):-M is 2*N,M =< 10.
my_sumlist12(A,B):-sumlist(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_lowercase14(A):-downcase_atom(A,A).
my_len15(A,B):-length(A,B).
my_last16(A,B):-last(A,B).
my_even17(A):-0 is A mod 2.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_flatten3/2).
prim(my_msort4/2).
prim(my_odd5/1).
prim(my_set6/1).
prim(my_pred7/2).
prim(my_max_list8/2).
prim(my_list_to_set9/2).
prim(my_succ10/2).
prim(my_double11/2).
prim(my_sumlist12/2).
prim(my_uppercase13/1).
prim(my_lowercase14/1).
prim(my_len15/2).
prim(my_last16/2).
prim(my_even17/1).
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
p(['a','B','C','N','C','p','n'],'C').
p(['e','U','p','e','Q'],'e').
p(['I','H','A','I','L','m'],'I').
p(['H','E','V','O','K','E','E','E'],'E').
p(['E','E','Z','t','M','l'],'E').
q(['n','t','E','n','I','l','i','a','R','q','Q'],'i').
q(['a','w','v','a','B','m'],'v').
q(['w','C','R','I','w','N','E','K','c','X'],'N').
q(['R','W','c','r','u','u','h','h','J'],'R').
q(['z','O','Z','y','C','z','e','k','c','e','M'],'Z').
