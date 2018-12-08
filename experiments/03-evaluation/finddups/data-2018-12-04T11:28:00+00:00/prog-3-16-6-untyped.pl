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
my_lowercase4(A):-downcase_atom(A,A).
my_list_to_set5(A,B):-list_to_set(A,B).
my_uppercase6(A):-upcase_atom(A,A).
my_reverse7(A,B):-reverse(A,B).
my_last8(A,B):-last(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_min_list10(A,B):-min_list(A,B).
my_msort11(A,B):-msort(A,B).
my_even12(A):-0 is A mod 2.
my_odd13(A):-1 is A mod 2.
my_tolower14(A,B):-downcase_atom(A,B).
my_toupper15(A,B):-upcase_atom(A,B).
my_flatten16(A,B):-flatten(A,B).
my_succ17(A,B):-succ(A,B),B =< 10.
my_double18(N,M):-M is 2*N,M =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_lowercase4/1).
prim(my_list_to_set5/2).
prim(my_uppercase6/1).
prim(my_reverse7/2).
prim(my_last8/2).
prim(my_sumlist9/2).
prim(my_min_list10/2).
prim(my_msort11/2).
prim(my_even12/1).
prim(my_odd13/1).
prim(my_tolower14/2).
prim(my_toupper15/2).
prim(my_flatten16/2).
prim(my_succ17/2).
prim(my_double18/2).
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
p(['p','f','s','X','h','X'],'X').
p(['k','r','P','G','r','Y','T','b'],'r').
p(['V','a','l','B','B','G','g','H','v'],'B').
p(['g','g','I','R','G','B','E','d'],'g').
p(['O','K','d','r','J','Q','O','s','O'],'O').
q(['o','u','S','y','d','S','w','O'],'w').
q(['i','S','d','d','z','q','c','Q'],'z').
q(['R','l','q','e','M','M','L'],'q').
q(['K','{','K','F','N','r','j','K','p','c','H'],'{').
q(['j','z','B','H','q','F','T','c','p','c'],'H').
