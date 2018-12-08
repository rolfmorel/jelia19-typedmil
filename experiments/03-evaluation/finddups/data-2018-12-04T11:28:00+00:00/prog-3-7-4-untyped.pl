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
my_sumlist3(A,B):-sumlist(A,B).
my_list_to_set4(A,B):-list_to_set(A,B).
my_odd5(A):-1 is A mod 2.
my_succ6(A,B):-succ(A,B),B =< 10.
my_pred7(A,B):-succ(B,A),A > 0.
my_max_list8(A,B):-max_list(A,B).
my_uppercase9(A):-upcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_sumlist3/2).
prim(my_list_to_set4/2).
prim(my_odd5/1).
prim(my_succ6/2).
prim(my_pred7/2).
prim(my_max_list8/2).
prim(my_uppercase9/1).
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
p(['j','T','W','W','q','v','y','k'],'W').
p(['O','v','n','v','J'],'v').
p(['A','W','g','c','W','m','O'],'W').
p(['L','Q','g','y','M','E','y','q'],'y').
p(['p','Q','h','y','o','F','Q','y'],'Q').
q(['N','Y','g','q','I','M','o','n','u','N'],'M').
q(['b','F','f','M','Z','q','M'],'F').
q(['F','K','F','y','P','a','A'],'P').
q(['T','s','r','T','i','V','r'],'i').
q(['b','n','O','u','G','b','H','x'],'u').
