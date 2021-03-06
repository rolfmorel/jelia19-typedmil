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
my_min_list3(A,B):-min_list(A,B).
my_last4(A,B):-last(A,B).
my_tolower5(A,B):-downcase_atom(A,B).
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_min_list3/2).
prim(my_last4/2).
prim(my_tolower5/2).
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
p(['O','E','C','l','E','c','o'],'E').
p(['F','M','g','U','F','b','U','y','c','P'],'F').
p(['R','P','y','M','j','R','q','r','Z'],'R').
p(['b','m','m','o','b','Q','q'],'m').
p(['G','G','S','P','G','P'],'G').
q(['s','Y','q','V','Y','f','s','Z','S','D'],'f').
q(['U','B','i','c','r','r','R'],'c').
q(['q','Q','O','K','m','t','u','U','Y','O'],'m').
q(['C','c','S','H','l','i','H','m','G','I'],'i').
q(['F','i','Z','i','k','a','K','p'],'Z').
