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
my_reverse3(A,B):-reverse(A,B).
my_odd4(A):-1 is A mod 2.
my_min_list5(A,B):-min_list(A,B).
my_lowercase6(A):-downcase_atom(A,A).
my_last7(A,B):-last(A,B).
my_list_to_set8(A,B):-list_to_set(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_set10(A):-list_to_set(A,A).
my_double11(N,M):-M is 2*N,M =< 10.
my_toupper12(A,B):-upcase_atom(A,B).
my_uppercase13(A):-upcase_atom(A,A).
my_max_list14(A,B):-max_list(A,B).
my_len15(A,B):-length(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_reverse3/2).
prim(my_odd4/1).
prim(my_min_list5/2).
prim(my_lowercase6/1).
prim(my_last7/2).
prim(my_list_to_set8/2).
prim(my_succ9/2).
prim(my_set10/1).
prim(my_double11/2).
prim(my_toupper12/2).
prim(my_uppercase13/1).
prim(my_max_list14/2).
prim(my_len15/2).
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
p(['E','C','p','l','y','C'],'C').
p(['a','Q','I','g','L','Z','s','g'],'g').
p(['p','l','K','g','G','y','y'],'y').
p(['q','o','D','b','S','J','o'],'o').
p(['X','a','p','L','b','s','n','w','g','g'],'g').
q(['o','V','V','z','s','f'],'o').
q(['m','m','v','F','U','x','Y','Q'],'v').
q(['D','y','f','R','k','c','f'],'k').
q(['i','B','F','j','F','T','h','o','b','T'],'i').
q(['R','i','y','d','Z','y','E','r'],'Z').
