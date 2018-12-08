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
my_max_list3(A,B):-max_list(A,B).
my_len4(A,B):-length(A,B).
my_last5(A,B):-last(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_list_to_set7(A,B):-list_to_set(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_lowercase9(A):-downcase_atom(A,A).
my_tolower10(A,B):-downcase_atom(A,B).
my_even11(A):-0 is A mod 2.
my_sumlist12(A,B):-sumlist(A,B).
my_set13(A):-list_to_set(A,A).
my_flatten14(A,B):-flatten(A,B).
my_min_list15(A,B):-min_list(A,B).
my_odd16(A):-1 is A mod 2.
my_pred17(A,B):-succ(B,A),A > 0.
my_reverse18(A,B):-reverse(A,B).
my_msort19(A,B):-msort(A,B).
my_succ20(A,B):-succ(A,B),B =< 10.
my_uppercase21(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_max_list3/2).
prim(my_len4/2).
prim(my_last5/2).
prim(my_double6/2).
prim(my_list_to_set7/2).
prim(my_toupper8/2).
prim(my_lowercase9/1).
prim(my_tolower10/2).
prim(my_even11/1).
prim(my_sumlist12/2).
prim(my_set13/1).
prim(my_flatten14/2).
prim(my_min_list15/2).
prim(my_odd16/1).
prim(my_pred17/2).
prim(my_reverse18/2).
prim(my_msort19/2).
prim(my_succ20/2).
prim(my_uppercase21/1).
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
p(['i','j','I','B','c','d','I','H'],'I').
p(['G','r','e','l','I','T','o','r','k'],'r').
p(['O','B','g','q','H','J','A','B'],'B').
p(['a','l','E','w','r','d','a','M'],'a').
p(['j','y','y','j','W'],'j').
q(['t','I','A','n','t','y','V','s'],'s').
q(['c','r','e','m','J','i','Q','Q'],'e').
q(['z','i','h','z','K','t','T','e','z','C'],'i').
q(['N','j','N','P','I','Y','x','i','k','e'],'e').
q(['a','H','c','j','C','N','A','Q','H','Y'],'j').
