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
my_last3(A,B):-last(A,B).
my_msort4(A,B):-msort(A,B).
my_max_list5(A,B):-max_list(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_len7(A,B):-length(A,B).
my_double8(N,M):-M is 2*N,M =< 10.
my_set9(A):-list_to_set(A,A).
my_uppercase10(A):-upcase_atom(A,A).
my_lowercase11(A):-downcase_atom(A,A).
my_succ12(A,B):-succ(A,B),B =< 10.
my_pred13(A,B):-succ(B,A),A > 0.
my_odd14(A):-1 is A mod 2.
my_min_list15(A,B):-min_list(A,B).
my_sumlist16(A,B):-sumlist(A,B).
my_flatten17(A,B):-flatten(A,B).
my_reverse18(A,B):-reverse(A,B).
my_toupper19(A,B):-upcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_last3/2).
prim(my_msort4/2).
prim(my_max_list5/2).
prim(my_list_to_set6/2).
prim(my_len7/2).
prim(my_double8/2).
prim(my_set9/1).
prim(my_uppercase10/1).
prim(my_lowercase11/1).
prim(my_succ12/2).
prim(my_pred13/2).
prim(my_odd14/1).
prim(my_min_list15/2).
prim(my_sumlist16/2).
prim(my_flatten17/2).
prim(my_reverse18/2).
prim(my_toupper19/2).
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
p(['m','z','j','m','O','F','f'],'m').
p(['Q','g','r','n','F','g','n','f','D','Z'],'g').
p(['t','F','T','C','x','O','T'],'T').
p(['X','x','H','x','W','J','c','E','n'],'x').
p(['z','O','L','W','w','E','h','I','O','x'],'O').
q(['Y','j','l','Z','k','q','k'],'l').
q(['n','T','c','q','A','E','A','A','B'],'T').
q(['c','n','o','n','F','t','n'],'c').
q(['x','z','x','B','y','l','g','S','u','z'],'B').
q(['K','I','I','r','O','[','y','S','L','Y'],'[').
