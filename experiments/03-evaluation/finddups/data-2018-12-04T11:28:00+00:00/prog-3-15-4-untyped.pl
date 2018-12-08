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
my_min_list4(A,B):-min_list(A,B).
my_succ5(A,B):-succ(A,B),B =< 10.
my_flatten6(A,B):-flatten(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_even8(A):-0 is A mod 2.
my_max_list9(A,B):-max_list(A,B).
my_msort10(A,B):-msort(A,B).
my_last11(A,B):-last(A,B).
my_sumlist12(A,B):-sumlist(A,B).
my_reverse13(A,B):-reverse(A,B).
my_len14(A,B):-length(A,B).
my_set15(A):-list_to_set(A,A).
my_tolower16(A,B):-downcase_atom(A,B).
my_list_to_set17(A,B):-list_to_set(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_toupper3/2).
prim(my_min_list4/2).
prim(my_succ5/2).
prim(my_flatten6/2).
prim(my_pred7/2).
prim(my_even8/1).
prim(my_max_list9/2).
prim(my_msort10/2).
prim(my_last11/2).
prim(my_sumlist12/2).
prim(my_reverse13/2).
prim(my_len14/2).
prim(my_set15/1).
prim(my_tolower16/2).
prim(my_list_to_set17/2).
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
p(['L','S','c','E','L','g'],'L').
p(['o','b','O','E','S','o','T','g'],'o').
p(['H','r','b','B','O','k','r','E','A','L'],'r').
p(['T','o','Q','N','x','z','h','x','p'],'x').
p(['X','l','l','B','Q','L'],'l').
q(['O','L','x','g','Q','W','g','A'],'x').
q(['b','I','B','d','Z','I','V'],'B').
q(['b','I','t','f','k','w','k'],'f').
q(['x','K','R','M','q','d','x'],'q').
q(['y','n','l','m','d','Y','R','w','Q','R'],'m').
