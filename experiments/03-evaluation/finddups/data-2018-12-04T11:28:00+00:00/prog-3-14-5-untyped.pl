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
my_uppercase3(A):-upcase_atom(A,A).
my_double4(N,M):-M is 2*N,M =< 10.
my_sumlist5(A,B):-sumlist(A,B).
my_even6(A):-0 is A mod 2.
my_pred7(A,B):-succ(B,A),A > 0.
my_len8(A,B):-length(A,B).
my_min_list9(A,B):-min_list(A,B).
my_max_list10(A,B):-max_list(A,B).
my_toupper11(A,B):-upcase_atom(A,B).
my_last12(A,B):-last(A,B).
my_list_to_set13(A,B):-list_to_set(A,B).
my_msort14(A,B):-msort(A,B).
my_reverse15(A,B):-reverse(A,B).
my_set16(A):-list_to_set(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_uppercase3/1).
prim(my_double4/2).
prim(my_sumlist5/2).
prim(my_even6/1).
prim(my_pred7/2).
prim(my_len8/2).
prim(my_min_list9/2).
prim(my_max_list10/2).
prim(my_toupper11/2).
prim(my_last12/2).
prim(my_list_to_set13/2).
prim(my_msort14/2).
prim(my_reverse15/2).
prim(my_set16/1).
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
p(['y','p','o','Q','I','o','S','c','n','r'],'o').
p(['Z','V','X','d','V','K','k','N','d','k'],'V').
p(['g','V','l','T','V','W','o','W','l','D'],'l').
p(['o','Z','X','E','G','y','y','a','L','c'],'y').
p(['L','q','p','p','x','J'],'p').
q(['U','n','k','N','o','p','E','O','p'],'O').
q(['l','l','p','j','Q','r','e','V'],'p').
q(['G','J','I','K','i','z','l','J','b'],'K').
q(['q','C','B','q','N','S','h'],'C').
q(['D','y','r','F','D','d'],'y').
