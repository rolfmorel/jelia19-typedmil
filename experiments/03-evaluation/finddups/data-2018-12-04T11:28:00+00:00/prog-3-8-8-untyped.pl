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
my_double3(N,M):-M is 2*N,M =< 10.
my_toupper4(A,B):-upcase_atom(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_pred6(A,B):-succ(B,A),A > 0.
my_last7(A,B):-last(A,B).
my_uppercase8(A):-upcase_atom(A,A).
my_max_list9(A,B):-max_list(A,B).
my_min_list10(A,B):-min_list(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_double3/2).
prim(my_toupper4/2).
prim(my_sumlist5/2).
prim(my_pred6/2).
prim(my_last7/2).
prim(my_uppercase8/1).
prim(my_max_list9/2).
prim(my_min_list10/2).
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
p(['Q','H','V','H','G','a','z'],'H').
p(['y','E','h','E','G','U','W'],'E').
p(['H','I','d','M','x','z','I'],'I').
p(['J','L','b','d','d','b','D'],'d').
p(['E','e','S','F','m','S'],'S').
q(['G','e','G','n','w','e','N','I','p'],'N').
q(['K','G','z','j','t','g','j','B','M','m'],'G').
q(['p','R','h','M','E','h','q','Q','V'],'R').
q(['c','q','L','P','w','x','H','H'],'c').
q(['n','U','w','Q','A','A','A'],'w').
