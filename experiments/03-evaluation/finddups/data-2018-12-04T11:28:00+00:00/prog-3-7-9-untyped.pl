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
my_odd4(A):-1 is A mod 2.
my_msort5(A,B):-msort(A,B).
my_list_to_set6(A,B):-list_to_set(A,B).
my_last7(A,B):-last(A,B).
my_flatten8(A,B):-flatten(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
prim(my_tail0/2).
prim(my_head1/2).
prim(my_element2/2).
prim(my_len3/2).
prim(my_odd4/1).
prim(my_msort5/2).
prim(my_list_to_set6/2).
prim(my_last7/2).
prim(my_flatten8/2).
prim(my_pred9/2).
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
p(['d','X','N','X','h','d','U'],'d').
p(['m','u','r','u','a'],'u').
p(['l','P','R','R','H','R'],'R').
p(['A','s','I','F','g','F','i'],'F').
p(['E','D','t','R','S','D','Q','O'],'D').
q(['B','U','c','L','g','I','I','z','i','h','M'],'g').
q(['m','A','N','s','A','n','r','s','s','Q','l'],'m').
q(['U','X','L','S','V','Z','U','T'],'L').
q(['J','u','d','O','O','x','v','R','s','x','W'],'R').
q(['k','D','O','i','i','P','P'],'D').
