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
my_max_list4(A,B):-max_list(A,B).
my_msort5(A,B):-msort(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_tolower7(A,B):-downcase_atom(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_sumlist9(A,B):-sumlist(A,B).
my_last10(A,B):-last(A,B).
my_reverse11(A,B):-reverse(A,B).
my_min_list12(A,B):-min_list(A,B).
my_odd13(A):-1 is A mod 2.
my_list_to_set14(A,B):-list_to_set(A,B).
my_double15(N,M):-M is 2*N,M =< 10.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_even3/1).
prim(my_max_list4/2).
prim(my_msort5/2).
prim(my_pred6/2).
prim(my_tolower7/2).
prim(my_succ8/2).
prim(my_sumlist9/2).
prim(my_last10/2).
prim(my_reverse11/2).
prim(my_min_list12/2).
prim(my_odd13/1).
prim(my_list_to_set14/2).
prim(my_double15/2).
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
p(['H','H','Z','G','g'],'H').
p(['u','b','U','b','r','S','k'],'b').
p(['n','b','F','j','z','w','n'],'n').
p(['D','D','c','l','R','x','F'],'D').
p(['W','S','k','W','N'],'W').
q(['R','e','W','B','B','X','t','A','h','Q','Z'],'t').
q(['l','X','G','X','C','U','E'],'U').
q(['r','Q','Q','Z','C','S','I','a'],'C').
q(['e','W','X','P','y','d','A','S','O','P'],'X').
q(['H','p','f','A','h','E','m','O','O','y'],'m').
