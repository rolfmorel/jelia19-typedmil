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
my_uppercase5(A):-upcase_atom(A,A).
my_pred6(A,B):-succ(B,A),A > 0.
my_list_to_set7(A,B):-list_to_set(A,B).
my_reverse8(A,B):-reverse(A,B).
my_sumlist9(A,B):-sumlist(A,B).
my_set10(A):-list_to_set(A,A).
my_min_list11(A,B):-min_list(A,B).
my_lowercase12(A):-downcase_atom(A,A).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_max_list4/2).
prim(my_uppercase5/1).
prim(my_pred6/2).
prim(my_list_to_set7/2).
prim(my_reverse8/2).
prim(my_sumlist9/2).
prim(my_set10/1).
prim(my_min_list11/2).
prim(my_lowercase12/1).
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
p(['U','H','W','Z','U','j','s','s','G'],'s').
p(['j','q','C','C','m','r'],'C').
p(['F','Y','p','q','Q','j','Z','M','M','p'],'M').
p(['M','q','P','p','X','E','P','a','a','F'],'a').
p(['G','g','e','N','e','O'],'e').
q(['I','y','l','u','A','l','M','N','E','v','W'],'N').
q(['k','w','E','N','W','C','F','Z','m','F'],'m').
q(['Q','T','A','h','c','A','e','X','J','u','N'],'N').
q(['T','W','N','I','h','n','g','O','B','B'],'O').
q(['h','L','z','G','V','K','K','U'],'h').
